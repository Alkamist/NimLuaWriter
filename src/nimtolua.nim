import std/[macros, tables, sequtils], lua

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
  SpecialExprs = {nnkCaseStmt, nnkIfExpr, nnkBlockExpr, nnkStmtListExpr}
  NimLuaFriendlyKinds = [nnkEmpty, nnkIdent, nnkSym]
  NimLiteralsToStrs = {
    nnkCharLit: "char",
    nnkIntLit: "int", nnkInt8Lit: "int8", nnkInt16Lit: "int16",
    nnkInt32Lit: "int32", nnkInt64Lit: "int64",
    nnkUIntLit: "uint", nnkUInt8Lit: "uint8", nnkUInt16Lit: "uint16",
    nnkUInt32Lit: "uint32", nnkUInt64Lit: "uint64",
    nnkFloatLit: "float", nnkFloat32Lit: "float32",
    nnkFloat64Lit: "float64", nnkFloat128Lit: "float128",
    nnkStrLit: "string", nnkRStrLit: "string", nnkTripleStrLit: "string",
    nnkNilLit: "nil",
  }.toTable
  NimLiteralKinds = NimLiteralsToStrs.unpackKeys
  NimTypeStrs = NimLiteralsToStrs.unpackValues.deduplicate
  IntTypeStrs = NimLiteralsToStrs.unpackValues({nnkIntLit..nnkInt64Lit})
  UIntTypeStrs = NimLiteralsToStrs.unpackValues({nnkUIntLit..nnkUInt64Lit})
  FloatTypeStrs = NimLiteralsToStrs.unpackValues({nnkFloatLit..nnkFloat128Lit})
  BoolTypeStr = "bool"
  BoolValues = ["true", "false"]

####################################################################

proc nnkCaseStmtToIfStmt(n: NimNode): NimNode =
  result = nnkIfStmt.newTree()

  for branch in n[1 ..^ 1]:
    if branch.kind == nnkOfBranch:
      result.add nnkElifBranch.newTree(
        nnkInfix.newTree(
          ident("=="),
          n[0],
          branch[0],
        ),
        branch[1],
      )
    else:
      result.add branch

  for i, child in result:
    if child.kind == nnkCaseStmt:
      result[i] = child.nnkCaseStmtToIfStmt()

proc containsSpecialExpr(n: NimNode): bool =
  if n.kind in SpecialExprs:
    return true
  for child in n:
    if child.containsSpecialExpr():
      return true

proc convertToExpression(n: NimNode; varSym: NimNode): NimNode =
  case n.kind:

  of nnkIfExpr, nnkIfStmt:
    result = nnkIfStmt.newTree()
    for branch in n:
      result.add branch.convertToExpression(varSym)

  of nnkBlockExpr, nnkBlockStmt,
     nnkStmtListExpr, nnkStmtList,
     nnkElifExpr, nnkElifBranch,
     nnkElseExpr, nnkElse:
    case n.kind:
    of nnkBlockExpr, nnkBlockStmt: result = nnkBlockStmt.newTree()
    of nnkStmtListExpr, nnkStmtList: result = nnkStmtList.newTree()
    of nnkElifExpr, nnkElifBranch: result = nnkElifBranch.newTree()
    of nnkElseExpr, nnkElse: result = nnkElse.newTree()
    else: discard

    let lastId = n.len - 1
    for i, child in n:
      if i == lastId:
        result.add child.convertToExpression(varSym)
      else:
        result.add child

  of nnkCaseStmt:
    result = n.nnkCaseStmtToIfStmt().convertToExpression(varSym)

  else:
    result = nnkAsgn.newTree(varSym, n)

proc specialExprResolution(n: NimNode, varSym: NimNode): (NimNode, NimNode) =
  if n.kind == nnkInfix:
    var
      exprResolutions = nnkStmtList.newTree()
      exprAssignments = n.kind.newTree(n[0])

    for term in n[1..2]:
      let (termResolutions, termAssignments) = term.specialExprResolution(varSym)
      exprResolutions.add termResolutions
      exprAssignments.add termAssignments

    result = (exprResolutions, exprAssignments)
  else:
    let exprSym = nskVar.genSym(varSym.strVal)

    var exprResolutions = nnkStmtList.newTree(
      nnkVarSection.newTree(
        nnkIdentDefs.newTree(
          exprSym,
          varSym.getType(),
          newEmptyNode(),
        )
      ),
      n.convertToExpression(exprSym),
    )

    result = (exprResolutions, exprSym)

####################################################################

converter toLuaNode(n: NimNode): LuaNode

proc toLuaOperator(n: NimNode): LuaNode =
  luaIdent(
    case n.strVal:
    of "!=": lokNotEquals.toString
    else: n.strVal
  )

proc nnkStmtListToLuaNode(n: NimNode): LuaNode =
  result = luaStmtList()
  for child in n:
    result.add child

proc nnkIfStmtToLuaNode(n: NimNode): LuaNode =
  result = luaIfStmt()
  for child in n:
    result.add child

proc nnkElifBranchToLuaNode(n: NimNode): LuaNode =
  result = luaElseIfBranch()
  for child in n:
    result.add child

proc nnkElseToLuaNode(n: NimNode): LuaNode =
  result = luaElseBranch()
  for child in n:
    result.add child

# proc nnkConvToLuaNode(n: NimNode): LuaNode =
#   let toType = n[0].strVal

#   case toType:
#   of IntTypeStrs, UIntTypeStrs, FloatTypeStrs, BoolTypeStr:
#     let fromType = n[1].nimTypeStr

#     if fromType in IntTypeStrs and toType == BoolTypeStr or
#        fromType in UIntTypeStrs and toType == BoolTypeStr or
#        fromType in FloatTypeStrs and toType == BoolTypeStr or
#        fromType in FloatTypeStrs and toType in IntTypeStrs or
#        fromType == BoolTypeStr and toType in IntTypeStrs or
#        fromType == BoolTypeStr and toType in UIntTypeStrs or
#        fromType == BoolTypeStr and toType in FloatTypeStrs:
#       result = luaCall(luaIdent(fromType & "_to_" & toType), n[1])
#     else:
#       result = n[1]
#   else:
#     result = n[1]

proc letOrVarSectionToLuaFriendly(n: NimNode, sectionKind: NimNodeKind): LuaNode =
  result = luaStmtList()
  for identDefs in n:
    let value = identDefs[^1]
    for varSym in identDefs[0 ..^ 3]:
      if value.containsSpecialExpr():
        let (exprResolutions, exprAssignments) = value.specialExprResolution(varSym)
        result.add exprResolutions
        result.add luaLocal(luaAsgn(varSym, exprAssignments))
      else:
        if value.kind == nnkEmpty:
          result.add luaLocal(varSym)
        else:
          result.add luaLocal(luaAsgn(varSym, value))

converter toLuaNode(n: NimNode): LuaNode =
  case n.kind
  of nnkEmpty, nnkIncludeStmt, nnkConstSection: luaEmpty()
  of nnkIdent, nnkSym: luaIdent(n.strVal)
  of nnkCharLit..nnkUInt64Lit: luaIntLit(n.intVal.int)
  of nnkFloatLit..nnkFloat64Lit: luaFloatLit(n.floatVal.float)
  of nnkStrLit..nnkTripleStrLit: luaStrLit(n.strVal)
  of nnkDiscardStmt: n[0]
  of nnkStmtList: n.nnkStmtListToLuaNode()
  of nnkLetSection, nnkVarSection: n.letOrVarSectionToLuaFriendly(n.kind)
  of nnkCaseStmt: n.nnkCaseStmtToIfStmt()
  of nnkIfStmt: n.nnkIfStmtToLuaNode()
  of nnkElifBranch: n.nnkElifBranchToLuaNode()
  of nnkElse: n.nnkElseToLuaNode()
  of nnkAsgn: luaAsgn(n[0], n[1])
  of nnkInfix: luaInfix(n[0], n[1], n[2])
  else: raise newException(IOError, "Unsupported NimNode kind: " & $n.kind)

macro writeLua*(indentationSpaces: static[int], n: typed): untyped =
  echo n.treeRepr
  let luaCode = n.toLuaNode().toLua(indentationSpaces)
  result = newStmtList(newStrLitNode(luaCode))