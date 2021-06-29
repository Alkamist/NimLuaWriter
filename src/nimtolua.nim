import
  std/macros,
  lua

const
  SupportedNimNodeKinds = {nnkEmpty, nnkSym, nnkIntLit, nnkFloatLit,
                           nnkStrLit, nnkStmtList, nnkStmtListExpr,
                           nnkBlockExpr, nnkIncludeStmt,
                           nnkLetSection, nnkVarSection,
                           nnkIfStmt, nnkIfExpr, nnkElifBranch,
                           nnkElifExpr, nnkElse, nnkElseExpr,
                           nnkInfix, nnkAsgn, nnkTypeSection,
                           nnkTypeDef, nnkCaseStmt}

proc toLuaNode*(n: NimNode): LuaNode

######################################################################
# Helpers
######################################################################

proc nnkIdentDefsAssignments(n: NimNode): LuaNode =
  result = lnkStmtList.newLuaTree()

  let defaultValue = n[n.len - 1]

  for i in 0 ..< n.len - 2:
    let variableIdent = n[i]

    if defaultValue.kind in [nnkEmpty, nnkCaseStmt, nnkIfExpr, nnkStmtListExpr, nnkBlockExpr]:
      result.add(variableIdent.toLuaNode)

    else:
      result.add(lnkInfix.newLuaTree(
        newLuaIdentNode(lokEquals.toString),
        variableIdent.toLuaNode,
        defaultValue.toLuaNode,
      ))

proc convertToExpression(n: LuaNode, varName: string): LuaNode =
  var expression = n

  case expression.kind:

  of lnkStmtList:
    let lastId = expression.len - 1
    if lastId >= 0:
      if expression[lastId].kind in [lnkStmtList, lnkIfStmt]:
        expression[lastId] = expression[lastId].convertToExpression(varName)
      else:
        expression[lastId] = lnkInfix.newLuaTree(
          newLuaIdentNode(lokEquals.toString),
          newLuaIdentNode(varName),
          expression[lastId],
        )

  of lnkIfStmt:
    for i, branch in expression:
      let lastId = branch.len - 1
      if lastId >= 0:
        if expression[lastId].kind in [lnkStmtList, lnkIfStmt]:
          expression[i][lastId] = branch[lastId].convertToExpression(varName)
        else:
          expression[i][lastId] = lnkInfix.newLuaTree(
            newLuaIdentNode(lokEquals.toString),
            newLuaIdentNode(varName),
            branch[lastId],
          )

  else:
    expression = lnkInfix.newLuaTree(
      newLuaIdentNode(lokEquals.toString),
      newLuaIdentNode(varName),
      expression,
    )

  result = lnkDoStmt.newLuaTree(expression)

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

proc nnkStmtListExprToLuaNode(n: NimNode): LuaNode =
  n.nnkStmtListToLuaNode

proc nnkBlockExprToLuaNode(n: NimNode): LuaNode =
  n[1].toLuaNode

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
        result.add(identDef[2].toLuaNode.convertToExpression(varName))

proc nnkVarSectionToLuaNode(n: NimNode): LuaNode =
  n.nnkLetSectionToLuaNode

proc nnkIfStmtToLuaNode(n: NimNode): LuaNode =
  result = lnkIfStmt.newLuaTree()
  for child in n:
    result.add(child.toLuaNode)

proc nnkIfExprToLuaNode(n: NimNode): LuaNode =
  n.nnkIfStmtToLuaNode

proc nnkElifBranchToLuaNode(n: NimNode): LuaNode =
  result = lnkElseIfBranch.newLuaTree()
  for child in n:
    result.add(child.toLuaNode)

proc nnkElifExprToLuaNode(n: NimNode): LuaNode =
  n.nnkElifBranchToLuaNode

proc nnkElseToLuaNode(n: NimNode): LuaNode =
  lnkElseBranch.newLuaTree(n[0].toLuaNode)

proc nnkElseExprToLuaNode(n: NimNode): LuaNode =
  n.nnkElseToLuaNode

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

proc nnkTypeSectionToLuaNode(n: NimNode): LuaNode =
  n[0].toLuaNode

proc nnkTypeDefToLuaNode(n: NimNode): LuaNode =
  n[0].toLuaNode

proc nnkCaseStmtToLuaNode(n: NimNode): LuaNode =
  result = lnkIfStmt.newLuaTree()
  let selector = n[0]
  for i in 1 .. n.len - 1:
    if n[i].kind == nnkOfBranch:
      var branch = n[i].nnkElifBranchToLuaNode
      branch[0] = lnkInfix.newLuaTree(
        newLuaIdentNode(lokEqualsEquals.toString),
        selector.toLuaNode,
        branch[0],
      )
      result.add(branch)
    else:
      result.add(n[i].toLuaNode)

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