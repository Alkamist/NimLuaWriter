import
  compiler/types,
  std/tables,
  hnimast/[compiler_aux, hast_common],
  lua

proc unpackKeys[K, V](t: Table[K, V]): seq[K] =
  for key in t.keys:
    result.add(key)

proc unpackValues[K, V](t: Table[K, V]): seq[V] =
  for value in t.values:
    result.add(value)

proc unpackValues[K, V](t: Table[K, V], keySet: set[K]): seq[V] =
  for key in keySet:
    result.add(t[key])

const
  SpecialExprs = {nkCaseStmt, nkIfExpr, nkBlockExpr, nkStmtListExpr, nkObjConstr}
  NimLiteralsToStrs = {
    nkCharLit: "char",
    nkIntLit: "int", nkInt8Lit: "int8", nkInt16Lit: "int16",
    nkInt32Lit: "int32", nkInt64Lit: "int64",
    nkUIntLit: "uint", nkUInt8Lit: "uint8", nkUInt16Lit: "uint16",
    nkUInt32Lit: "uint32", nkUInt64Lit: "uint64",
    nkFloatLit: "float", nkFloat32Lit: "float32",
    nkFloat64Lit: "float64", nkFloat128Lit: "float128",
    nkStrLit: "string", nkRStrLit: "string", nkTripleStrLit: "string",
    nkNilLit: "nil",
  }.toTable
  # NimLiteralKinds = NimLiteralsToStrs.unpackKeys
  # NimTypeStrs = NimLiteralsToStrs.unpackValues.deduplicate
  IntTypeStrs = NimLiteralsToStrs.unpackValues({nkIntLit..nkInt64Lit})
  UIntTypeStrs = NimLiteralsToStrs.unpackValues({nkUIntLit..nkUInt64Lit})
  FloatTypeStrs = NimLiteralsToStrs.unpackValues({nkFloatLit..nkFloat128Lit})
  BoolTypeStr = "bool"
  # BoolValues = ["true", "false"]

type
  NimProgramState = object
    tempVarCount: int

var s: NimProgramState

proc genTempVarName(s: var NimProgramState): string =
  result = "TEMP_" & $s.tempVarCount
  s.tempVarCount += 1

converter toLuaNode(n: PNode): LuaNode

####################################################################

proc luaDefaultValue(n: PNode): LuaNode =
  case n.typ.kind:
  of tyFloat..tyFloat128: luaFloatLit(0.0)
  of tyInt..tyInt64: luaIntLit(0)
  of tyUInt..tyUInt64: luaIntLit(0)
  of tyEnum, tyString, tyCString: luaStrLit("")
  of tyObject: luaCall(luaIdent($n.typ & "_init"))
  else: raise newException(IOError, "Unsupported type kind for default value: " & $n.typ.kind)

proc containsSpecialExpr(n: PNode): bool =
  if n.kind in SpecialExprs:
    return true
  for child in n:
    if child.containsSpecialExpr():
      return true

proc convertToExpression(n: LuaNode, varName: string): LuaNode =
  case n.kind:

  of lnkIfStmt:
    result = luaIfStmt()
    for branch in n:
      result.add branch.convertToExpression(varName)

  of lnkStmtList, lnkDoStmt, lnkElseIfBranch, lnkElseBranch:
    result = n.kind.luaTree()

    let lastId = n.len - 1
    for i, child in n:
      if i == lastId:
        result.add child.convertToExpression(varName)
      else:
        result.add child

  else:
    result = luaAsgn(luaIdent(varName), n)

proc specialExprResolution(n: PNode, varSym: PNode): (LuaNode, LuaNode) =
  if n.kind == nkInfix:
    var
      exprResolutions = luaStmtList()
      exprAssignments = luaInfix(n[0])

    let (leftResolutions, leftAssignments) = n[1].specialExprResolution(varSym)
    exprResolutions.add leftResolutions
    exprAssignments.add leftAssignments

    let (rightResolutions, rightAssignments) = n[2].specialExprResolution(varSym)
    exprResolutions.add rightResolutions
    exprAssignments.add rightAssignments

    result = (exprResolutions, exprAssignments)
  else:
    let exprSym = luaIdent(s.genTempVarName())

    var exprResolutions =
      if n.kind == nkObjConstr:
        var output = luaStmtList(
          luaLocal(luaAsgn(
            exprSym,
            luaCall(luaIdent($n.typ & "_init")),
          )),
        )

        for field in n[1 ..^ 1]:
          output.add luaAsgn(luaDotExpr(exprSym, field[0]), field[1])

        output
      else:
        luaStmtList(
          luaLocal(exprSym),
          n.convertToExpression(exprSym.strVal),
        )

    result = (exprResolutions, exprSym)

####################################################################

proc toLuaOperator(n: PNode): LuaNode =
  luaIdent(
    case $n:
    of "!=": lokNotEquals.toString
    else: $n
  )

proc nkSymToLuaNode(n: PNode): LuaNode =
  if n.sym.kind == skEnumField:
    luaStrLit($n)
  else:
    luaIdent($n)

proc nkLetOrVarSectionToLuaNode(n: PNode, sectionKind: TNodeKind): LuaNode =
  result = luaStmtList()
  for identDefs in n:
    let value = identDefs[^1]
    for varSym in identDefs[0 ..^ 3]:
      if value.containsSpecialExpr():
        let (exprResolutions, exprAssignments) = value.specialExprResolution(varSym)
        result.add exprResolutions
        result.add luaLocal(luaAsgn(varSym, exprAssignments))
      else:
        if value.kind == nkEmpty:
          result.add luaLocal(varSym)
        else:
          result.add luaLocal(luaAsgn(varSym, value))

proc nkCaseStmtToLuaNode(n: PNode): LuaNode =
  result = luaIfStmt()

  for branch in n[1 ..^ 1]:
    if branch.kind == nkOfBranch:
      result.add luaElseIfBranch(
        luaInfix(luaIdent("=="), n[0], branch[0]),
        branch[1],
      )
    else:
      result.add branch

proc nkConvToLuaNode(n: PNode): LuaNode =
  let toType = $n[0]

  case toType:
  of IntTypeStrs, UIntTypeStrs, FloatTypeStrs, BoolTypeStr:
    let fromType = $n[1].typ

    if fromType in IntTypeStrs and toType == BoolTypeStr or
       fromType in UIntTypeStrs and toType == BoolTypeStr or
       fromType in FloatTypeStrs and toType == BoolTypeStr or
       fromType in FloatTypeStrs and toType in IntTypeStrs or
       fromType == BoolTypeStr and toType in IntTypeStrs or
       fromType == BoolTypeStr and toType in UIntTypeStrs or
       fromType == BoolTypeStr and toType in FloatTypeStrs:
      result = luaCall(luaIdent(fromType & "_to_" & toType), n[1])
    else:
      result = n[1]
  else:
    result = n[1]

proc nkTypeDefToLuaNode(n: PNode): LuaNode =
  case n[2].typ.kind:
  of tyEnum: result = luaEmpty()
  of tyObject:
    var fnBody = luaStmtList(luaLocal(luaAsgn(luaIdent("self"), luaTableDef())))

    for identDefs in n[2][2]:
      for varSym in identDefs[0 ..^ 3]:
        fnBody.add luaAsgn(
          luaDotExpr(luaIdent("self"), varSym),
          identDefs[^2].luaDefaultValue(),
        )

    fnBody.add luaReturnStmt(luaIdent("self"))

    result = luaLocal(luaAsgn(
      luaIdent($n[0] & "_init"),
      luaFnDef(
        luaFnParams(),
        fnBody,
      ),
    ))
  else: raise newException(IOError, "Unsupported type kind for type def: " & $n[0].typ.kind)

template unpackTo(luaTreeFn: untyped): untyped =
  result = luaTreeFn()
  for child in n:
    result.add child

proc nkStmtListToLuaNode(n: PNode): LuaNode = unpackTo(luaStmtList)
proc nkIfStmtToLuaNode(n: PNode): LuaNode = unpackTo(luaIfStmt)
proc nkElifBranchToLuaNode(n: PNode): LuaNode = unpackTo(luaElseIfBranch)
proc nkElseToLuaNode(n: PNode): LuaNode = unpackTo(luaElseBranch)
proc nkTypeSectionToLuaNode(n: PNode): LuaNode = unpackTo(luaStmtList)

converter toLuaNode(n: PNode): LuaNode =
  case n.kind
  of nkEmpty, nkIncludeStmt, nkConstSection: luaEmpty()
  of nkIdent: luaIdent($n)
  of nkCharLit..nkUInt64Lit: luaIntLit(n.intVal.int)
  of nkFloatLit..nkFloat64Lit: luaFloatLit(n.floatVal.float)
  of nkStrLit..nkTripleStrLit: luaStrLit(n.strVal)
  of nkDiscardStmt: n[0]
  of nkAsgn: luaAsgn(n[0], n[1])
  of nkInfix: luaInfix(n[0], n[1], n[2])
  of nkSym: n.nkSymToLuaNode()
  of nkTypeSection: n.nkTypeSectionToLuaNode()
  of nkStmtList: n.nkStmtListToLuaNode()
  of nkLetSection, nkVarSection: n.nkLetOrVarSectionToLuaNode(n.kind)
  of nkCaseStmt: n.nkCaseStmtToLuaNode()
  of nkIfStmt, nkIfExpr: n.nkIfStmtToLuaNode()
  of nkElifBranch, nkElifExpr: n.nkElifBranchToLuaNode()
  of nkElse, nkElseExpr: n.nkElseToLuaNode()
  of nkConv: n.nkConvToLuaNode()
  of nkTypeDef: n.nkTypeDefToLuaNode()
  else: raise newException(IOError, "Unsupported PNode kind: " & $n.kind)

proc writeLua*(nimCode: string, indentationSpaces: int): string =
  let n = nimCode.compileString()
  echo n.treeRepr()
  n.toLuaNode().toLua(indentationSpaces)