import
  #compiler/[sighashes],
  std/[tables, sequtils],
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
  SpecialExprs = {nkCaseStmt, nkIfExpr, nkBlockExpr, nkStmtListExpr}
  NimLuaFriendlyKinds = [nkEmpty, nkIdent, nkSym]
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
  NimLiteralKinds = NimLiteralsToStrs.unpackKeys
  NimTypeStrs = NimLiteralsToStrs.unpackValues.deduplicate
  IntTypeStrs = NimLiteralsToStrs.unpackValues({nkIntLit..nkInt64Lit})
  UIntTypeStrs = NimLiteralsToStrs.unpackValues({nkUIntLit..nkUInt64Lit})
  FloatTypeStrs = NimLiteralsToStrs.unpackValues({nkFloatLit..nkFloat128Lit})
  BoolTypeStr = "bool"
  BoolValues = ["true", "false"]

converter toLuaNode(n: PNode): LuaNode

####################################################################

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
    #let exprSym = nskVar.genSym(varSym.strVal)
    let exprSym = luaIdent("TEMP")

    var exprResolutions = luaStmtList(
      luaLocal(exprSym),
      n.convertToExpression(exprSym.strVal),
    )

    result = (exprResolutions, exprSym)

####################################################################

# proc toLuaOperator(n: PNode): LuaNode =
#   luaIdent(
#     case n.strVal:
#     of "!=": lokNotEquals.toString
#     else: n.strVal
#   )

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

template unpackTo(luaTreeFn: untyped): untyped =
  result = luaTreeFn()
  for child in n:
    result.add child

proc nkStmtListToLuaNode(n: PNode): LuaNode = unpackTo(luaStmtList)
proc nkIfStmtToLuaNode(n: PNode): LuaNode = unpackTo(luaIfStmt)
proc nkElifBranchToLuaNode(n: PNode): LuaNode = unpackTo(luaElseIfBranch)
proc nkElseToLuaNode(n: PNode): LuaNode = unpackTo(luaElseBranch)

converter toLuaNode(n: PNode): LuaNode =
  case n.kind
  of nkEmpty, nkIncludeStmt, nkConstSection: luaEmpty()
  of nkIdent: luaIdent(n.strVal)
  of nkSym: luaIdent($n)
  of nkCharLit..nkUInt64Lit: luaIntLit(n.intVal.int)
  of nkFloatLit..nkFloat64Lit: luaFloatLit(n.floatVal.float)
  of nkStrLit..nkTripleStrLit: luaStrLit(n.strVal)
  of nkDiscardStmt: n[0]
  of nkStmtList: n.nkStmtListToLuaNode()
  of nkLetSection, nkVarSection: n.nkLetOrVarSectionToLuaNode(n.kind)
  of nkCaseStmt: n.nkCaseStmtToLuaNode()
  of nkIfStmt, nkIfExpr: n.nkIfStmtToLuaNode()
  of nkElifBranch, nkElifExpr: n.nkElifBranchToLuaNode()
  of nkElse, nkElseExpr: n.nkElseToLuaNode()
  of nkAsgn: luaAsgn(n[0], n[1])
  of nkInfix: luaInfix(n[0], n[1], n[2])
  else: raise newException(IOError, "Unsupported PNode kind: " & $n.kind)

proc writeLua*(nimCode: string, indentationSpaces: int): string =
  let n = nimCode.compileString()
  echo n.treeRepr()
  n.toLuaNode().toLua(indentationSpaces)