import
  std/macros,
  lua

const
  DoExprs = {nnkCaseStmt, nnkIfExpr, nnkStmtListExpr, nnkBlockExpr}
  SupportedNimNodeKinds = {nnkEmpty, nnkSym, nnkIdent, nnkIntLit, nnkFloatLit,
                           nnkStrLit, nnkStmtList, nnkStmtListExpr,
                           nnkBlockExpr, nnkIncludeStmt,
                           nnkLetSection, nnkVarSection,
                           nnkIfStmt, nnkIfExpr, nnkElifBranch,
                           nnkElifExpr, nnkElse, nnkElseExpr,
                           nnkInfix, nnkAsgn, nnkTypeSection,
                           nnkTypeDef, nnkCaseStmt, nnkProcDef,
                           nnkReturnStmt, nnkFormalParams, nnkDiscardStmt,
                           nnkCall}

proc toLuaNode*(n: NimNode): LuaNode

######################################################################
# Helpers
######################################################################

proc nnkIdentDefsAssignments(n: NimNode): LuaNode =
  result = lnkStmtList.newLuaTree()

  let defaultValue = n[n.len - 1]

  for i in 0 ..< n.len - 2:
    let variableIdent = n[i]

    if defaultValue.kind == nnkEmpty or
       defaultValue.kind in DoExprs:
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

proc nnkStmtListHasEarlyReturn(n: NimNode): bool =
  for child in n:
    case child.kind:
    of nnkReturnStmt: return true
    of nnkStmtList: result = child.nnkStmtListHasEarlyReturn
    else: discard

proc nnkProcDefHasEarlyReturn(n: NimNode): bool =
  let body = n[6]
  case body.kind:
  of nnkReturnStmt: true
  of nnkStmtList: body.nnkStmtListHasEarlyReturn
  else: false

proc luaDefaultValueInit(variable: LuaNode, defaultValue: LuaNode): LuaNode =
  result = lnkIfStmt.newLuaTree(
    lnkElseIfBranch.newLuaTree(
      lnkInfix.newLuaTree(
        newLuaIdentNode(lokEqualsEquals.toString),
        variable,
        newLuaNilLitNode(),
      ),
      lnkInfix.newLuaTree(
        newLuaIdentNode(lokEquals.toString),
        variable,
        defaultValue,
      ),
    ),
  )

proc nnkFormalParamsProcDefDefaults(n: NimNode): LuaNode =
  result = lnkStmtList.newLuaTree()

  let lastIdentDefId = n.len - 1
  for identDefId in 1 .. lastIdentDefId:
    let
      identDef = n[identDefId]
      defaultValue = identDef[identDef.len - 1]
      lastVarNameId = identDef.len - 3

    for varNameId in 0 .. lastVarNameId:
      let varName = identDef[varNameId]
      result.add(luaDefaultValueInit(varName.toLuaNode, defaultValue.toLuaNode))

proc nnkIdentDefVars(n: NimNode): seq[NimNode] =
  for i in 0 ..< n.len - 2:
    result.add(n[i])

proc nnkIdentDefType(n: NimNode): NimNode =
  n[n.len - 2]

proc nnkIdentDefValue(n: NimNode): NimNode =
  n[n.len - 1]

# proc nnkObjConstrAssignments(n: NimNode, varName: string): LuaNode =
#   result = lnkStmtList.newLuaTree(
#     lnkCall.newLuaTree(n[0].toLuaNode),
#   )

#   for i in 1 .. n.len - 1:
#     let
#       memberName = n[i][0].toLuaNode
#       memberValue = n[i][1].toLuaNode

#     result.add(
#       lnkInfix.newLuaTree(
#         newLuaIdentNode(lokEquals.toString),
#         lnkDotExpr.newLuaTree(varName, memberName),
#         memberValue,
#       ),
#     )

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
  if n[1].kind in DoExprs:
    result = lnkStmtList.newLuaTree()
    result.add(n[1].toLuaNode.convertToExpression(n[0].strVal))
  else:
    result = lnkInfix.newLuaTree(
      newLuaIdentNode(lokEquals.toString),
      n[0].toLuaNode,
      n[1].toLuaNode,
    )

proc nnkTypeSectionToLuaNode(n: NimNode): LuaNode =
  result = lnkStmtList.newLuaTree()
  for child in n:
    result.add(child.toLuaNode)

proc nnkTypeDefToLuaNode(n: NimNode): LuaNode =
  let
    typeName = n[0]
    objectTy = n[2]
    recList = objectTy[2]

  var tableDef = lnkTableDef.newLuaTree()

  for identDef in recList:
    let defaultValue = identDef.nnkIdentDefValue
    for varName in identDef.nnkIdentDefVars:
      if defaultValue.kind != nnkEmpty:
        tableDef.add(lnkInfix.newLuaTree(
          newLuaIdentNode(lokEquals.toString),
          varName.toLuaNode,
          defaultValue.toLuaNode,
        ))
      else:
        tableDef.add(varName.toLuaNode)

  var functionBody = lnkStmtList.newLuaTree(
    lnkLocal.newLuaTree(
      lnkInfix.newLuaTree(
        newLuaIdentNode(lokEquals.toString),
        newLuaIdentNode("self"),
        tableDef,
      ),
    ),
    lnkReturnStmt.newLuaTree(
      newLuaIdentNode("self"),
    ),
  )

  result = lnkLocal.newLuaTree(
    lnkInfix.newLuaTree(
      newLuaIdentNode(lokEquals.toString),
      typeName.toLuaNode,
      lnkFnDef.newLuaTree(
        lnkFnParams.newLuaTree(),
        functionBody,
      ),
    ),
  )

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

proc nnkProcDefToLuaNode(n: NimNode): LuaNode =
  let
    functionName = n[0].toLuaNode
    functionHasResult = n[3][0].kind != nnkEmpty

  var
    functionParams = n[3].toLuaNode
    functionBody = lnkStmtList.newLuaTree(n[3].nnkFormalParamsProcDefDefaults)

  if functionHasResult:
    let resultName = n[7].toLuaNode
    functionBody.add(lnkLocal.newLuaTree(resultName))

    if n.nnkProcDefHasEarlyReturn:
      functionBody.add(n[6].toLuaNode)
    else:
      functionBody.add(lnkDoStmt.newLuaTree(n[6].toLuaNode))
      functionBody.add(lnkReturnStmt.newLuaTree(resultName))

  else:
    functionBody.add(n[6].toLuaNode)

  result = lnkLocal.newLuaTree(
    lnkInfix.newLuaTree(
      newLuaIdentNode(lokEquals.toString),
      functionName,
      lnkFnDef.newLuaTree(functionParams, functionBody),
    ),
  )

proc nnkReturnStmtToLuaNode(n: NimNode): LuaNode =
  if n[0].kind == nnkAsgn:
    lnkReturnStmt.newLuaTree(n[0][1].toLuaNode)
  else:
    lnkReturnStmt.newLuaTree(n[0].toLuaNode)

proc nnkFormalParamsToLuaNode(n: NimNode): LuaNode =
  result = lnkFnParams.newLuaTree()
  let lastParamId = n.len - 1
  for paramId in 1 .. lastParamId:
    let identDef = n[paramId]
    for paramNameId in 0 .. identDef.len - 3:
      let paramName = identDef[paramNameId]
      result.add(paramName.toLuaNode)

proc nnkDiscardStmtToLuaNode(n: NimNode): LuaNode =
  n[0].toLuaNode

proc nnkCallToLuaNode(n: NimNode): LuaNode =
  result = lnkCall.newLuaTree()
  for child in n:
    result.add(child.toLuaNode)

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