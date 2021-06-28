import
  std/macros,
  lua

const
  SupportedNimNodeKinds = {nnkEmpty, nnkSym, nnkIntLit, nnkFloatLit,
                           nnkStrLit, nnkStmtList, nnkIncludeStmt,
                           nnkLetSection, nnkVarSection,
                           nnkIfStmt, nnkElifBranch, nnkElse,
                           nnkInfix, nnkAsgn}

proc toLuaNode*(n: NimNode): LuaNode
proc nnkElifBranchToLuaNode(n: NimNode): LuaNode
proc nnkElseToLuaNode(n: NimNode): LuaNode
proc nnkStmtListToLuaNode(n: NimNode): LuaNode

######################################################################
# Helpers
######################################################################

proc nnkIdentDefsAssignments(n: NimNode): LuaNode =
  result = lnkStmtList.newLuaTree()
  let defaultValue = n[n.len - 1]
  for i in 0 ..< n.len - 2:
    let variableIdent = n[i]
    if defaultValue.kind in [nnkEmpty, nnkIfExpr, nnkStmtListExpr, nnkBlockExpr]:
      result.add(variableIdent.toLuaNode)
    else:
      result.add(lnkInfix.newLuaTree(
        newLuaIdentNode(lokEquals.toString),
        variableIdent.toLuaNode,
        defaultValue.toLuaNode,
      ))

proc nnkIdentDefsIfExpr(n: NimNode): LuaNode =
  result = lnkIfStmt.newLuaTree()
  for branch in n:
    case branch.kind:
    of nnkElifExpr: result.add(branch.nnkElifBranchToLuaNode)
    of nnkElseExpr: result.add(branch.nnkElseToLuaNode)
    else: discard

proc nnkIdentDefsExpression(n: NimNode): LuaNode =
  result = lnkDoStmt.newLuaTree()
  let expression = n[2]
  case expression.kind:
  of nnkIfExpr: result.add(expression.nnkIdentDefsIfExpr)
  of nnkStmtListExpr: result.add(expression.nnkStmtListToLuaNode)
  of nnkBlockExpr: result.add(expression[1].nnkStmtListToLuaNode)
  else: result.add(expression.toLuaNode)

proc finalizeLuaExpression(n: LuaNode, varName: string): LuaNode =
  result = n
  if n[0].kind == lnkIfStmt:
    for i, branch in n[0]:
      let lastId = branch.len - 1
      result[0][i][lastId] = lnkInfix.newLuaTree(
        newLuaIdentNode(lokEquals.toString),
        newLuaIdentNode(varName),
        branch[lastId],
      )
  elif n[0].kind == lnkStmtList:
    let lastId = n[0].len - 1
    result[0][lastId] = lnkInfix.newLuaTree(
      newLuaIdentNode(lokEquals.toString),
      newLuaIdentNode(varName),
      result[0][lastId],
    )

######################################################################
# Nim Nodes
######################################################################

proc nnkEmptyToLuaNode(n: NimNode): LuaNode =
  newLuaEmptyNode()

proc nnkSymToLuaNode(n: NimNode): LuaNode =
  newLuaIdentNode(n.strVal)

proc nnkIdentToLuaNode(n: NimNode): LuaNode =
  newLuaIdentNode(n.strVal)

proc nnkIntLitToLuaNode(n: NimNode): LuaNode =
  newLuaIntLitNode(n.intVal.int)

proc nnkFloatLitToLuaNode(n: NimNode): LuaNode =
  newLuaFloatLitNode(n.floatVal.float)

proc nnkStrLitToLuaNode(n: NimNode): LuaNode =
  newLuaStrLitNode(n.strVal)

proc nnkStmtListToLuaNode(n: NimNode): LuaNode =
  result = lnkStmtList.newLuaTree()
  for child in n:
    result.add(child.toLuaNode)

proc nnkIncludeStmtToLuaNode(n: NimNode): LuaNode =
  newLuaEmptyNode()

proc nnkLetSectionToLuaNode(n: NimNode): LuaNode =
  result = lnkStmtList.newLuaTree()
  for identDef in n:
    let assignments = identDef.nnkIdentDefsAssignments
    result.add(lnkLocal.newLuaTree(assignments))
    for varId in 0 ..< identDef.len - 2:
      if assignments[varId].kind != lnkInfix:
        let varName = identDef[varId].strVal
        result.add(identDef.nnkIdentDefsExpression.finalizeLuaExpression(varName))

proc nnkVarSectionToLuaNode(n: NimNode): LuaNode =
  n.nnkLetSectionToLuaNode

proc nnkIfStmtToLuaNode(n: NimNode): LuaNode =
  result = lnkIfStmt.newLuaTree()
  for child in n:
    result.add(child.toLuaNode)

proc nnkElifBranchToLuaNode(n: NimNode): LuaNode =
  result = lnkElseIfBranch.newLuaTree()
  for child in n:
    result.add(child.toLuaNode)

proc nnkElseToLuaNode(n: NimNode): LuaNode =
  lnkElseBranch.newLuaTree(n[0].toLuaNode)

proc nnkInfixToLuaNode(n: NimNode): LuaNode =
  result = lnkInfix.newLuaTree()
  for child in n:
    result.add(child.toLuaNode)

proc nnkAsgnToLuaNode(n: NimNode): LuaNode =
  result = lnkInfix.newLuaTree(
    newLuaIdentNode(lokEquals.toString),
    n[0].toLuaNode,
    n[1].toLuaNode,
  )

######################################################################

macro defineToLuaNode(): untyped =
  result = nnkProcDef.newTree(
    nnkPostfix.newTree(ident("*"), ident("toLuaNode")),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      ident("LuaNode"),
      nnkIdentDefs.newTree(
        ident("n"),
        ident("NimNode"),
        newEmptyNode(),
      ),
    ),
    newEmptyNode(),
    newEmptyNode(),
    nnkStmtList.newTree(),
  )

  var cases = nnkCaseStmt.newTree(
    nnkDotExpr.newTree(
      ident("n"),
      ident("kind"),
    ),
  )

  for kind in SupportedNimNodeKinds:
    cases.add(nnkOfBranch.newTree(
      ident($kind),
      nnkCall.newTree(
        nnkDotExpr.newTree(
          ident("n"),
          ident($kind & "ToLuaNode"),
        ),
      ),
    ))

  let errorAst = quote do:
    raise newException(IOError, "Unsupported NimNode kind: " & $n.kind)

  cases.add(nnkElse.newTree(errorAst))

  result[6].add(cases)

defineToLuaNode()

macro writeLua*(indentationSpaces: static[int], code: typed): untyped =
  echo code.treeRepr
  let luaCode = code.toLuaNode.toLua(indentationSpaces)
  result = newStmtList(newStrLitNode(luaCode))