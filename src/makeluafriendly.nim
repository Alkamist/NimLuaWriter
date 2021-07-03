import std/[macros, tables, sequtils]

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
  IgnoreNodes = {nnkIncludeStmt, nnkConstSection}
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

proc toLuaFriendlyNode(n: NimNode): NimNode
proc convertToExpression(n: NimNode; varSym: NimNode): NimNode

proc nnkCaseStmtToLuaFriendly(n: NimNode): NimNode =
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
  result = result.toLuaFriendlyNode()

proc containsSpecialExpr(n: NimNode): bool =
  if n.kind in SpecialExprs:
    return true
  for child in n:
    if child.containsSpecialExpr():
      return true

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

proc letOrVarSectionToLuaFriendly(n: NimNode, sectionKind: NimNodeKind): NimNode =
  result = nnkStmtList.newTree()
  for identDefs in n:
    let value = identDefs[^1]
    for varSym in identDefs[0 ..^ 3]:
      if value.containsSpecialExpr():
        let (exprResolutions, exprAssignments) = value.specialExprResolution(varSym)

        result.add exprResolutions

        result.add sectionKind.newTree(
          nnkIdentDefs.newTree(
            varSym,
            newEmptyNode(),
            exprAssignments,
          )
        )
      else:
        result.add sectionKind.newTree(
          nnkIdentDefs.newTree(
            varSym,
            newEmptyNode(),
            value.toLuaFriendlyNode(),
          )
        )

proc convertToExpression(n: NimNode; varSym: NimNode): NimNode =
  case n.kind:

  of nnkIfExpr:
    result = nnkIfStmt.newTree()
    for branch in n:
      result.add branch.convertToExpression(varSym)

  of nnkBlockExpr, nnkStmtListExpr, nnkElifExpr, nnkElseExpr:
    case n.kind:
    of nnkBlockExpr: result = nnkBlockStmt.newTree()
    of nnkStmtListExpr: result = nnkStmtList.newTree()
    of nnkElifExpr: result = nnkElifBranch.newTree()
    of nnkElseExpr: result = nnkElse.newTree()
    else: discard

    let lastId = n.len - 1
    for i, child in n:
      if i == lastId:
        result.add child.convertToExpression(varSym)
      else:
        result.add child

  of nnkCaseStmt:
    result = n.nnkCaseStmtToLuaFriendly().convertToExpression(varSym)

  else:
    result = nnkAsgn.newTree(varSym, n)

proc toLuaFriendlyNode(n: NimNode): NimNode =
  case n.kind:
  of NimLuaFriendlyKinds, NimLiteralKinds: result = n.copyNimNode()
  of nnkLetSection: result = n.letOrVarSectionToLuaFriendly(nnkLetSection)
  of nnkVarSection: result = n.letOrVarSectionToLuaFriendly(nnkVarSection)
  of nnkCaseStmt: result = n.nnkCaseStmtToLuaFriendly()
  else:
    result = n.kind.newTree()
    for child in n:
      if child.kind notin IgnoreNodes:
        result.add child.toLuaFriendlyNode()

macro makeLuaFriendly*(n: typed): untyped =
  #echo n.treeRepr()
  echo n.toLuaFriendlyNode().treeRepr()
  n.toLuaFriendlyNode()