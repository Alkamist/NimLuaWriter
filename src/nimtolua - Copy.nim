import
  std/[macros, options, tables, strutils],
  lua,
  nimnodeshortcuts

const
  # AsgnShortcutOperators = ["+=", "-=", "*=", "/=", "%="]
  # SpecialExprs = {nnkCaseStmt, nnkIfExpr, nnkBlockExpr, nnkStmtListExpr}
  SupportedNimNodeKinds = {nnkEmpty, nnkSym, nnkIdent, nnkIntLit, nnkFloatLit,
                           nnkStrLit, nnkStmtList, nnkIncludeStmt,
                           nnkLetSection}
  # SupportedNimNodeKinds = {nnkEmpty, nnkSym, nnkIdent, nnkIntLit, nnkFloatLit,
  #                          nnkStrLit, nnkStmtList, nnkIncludeStmt,
  #                          nnkLetSection, nnkVarSection,
  #                          nnkInfix, nnkAsgn, nnkIdentDefs, nnkFormalParams,
  #                          nnkProcDef, nnkBlockStmt, nnkDiscardStmt,
  #                          nnkHiddenStdConv, nnkCall, nnkConv, nnkIfStmt,
  #                          nnkElifBranch, nnkElse, nnkCaseStmt, nnkIfExpr,
  #                          nnkElifExpr, nnkElseExpr, nnkBlockExpr, nnkStmtListExpr,
  #                          nnkCommand, nnkConstSection}

type
  NimProgramState = object
    isInFormalParams: bool
    procReturnTypeStrs: Table[string, string]
    varTypeStrs: Table[string, string]

######################################################################

proc toLuaNode*(s: var NimProgramState, n: NimNode): LuaNode
proc typeStr(s: var NimProgramState, n: NimNode): string
proc convertToExpression(n: LuaNode, varName: string): LuaNode

#####################################################################

proc nimStrToLuaOperatorKind(text: string): LuaOperatorKind =
  case text:
  of "!=": lokNotEquals
  else: parseEnum[LuaOperatorKind](text)

proc luaDefaultValueInit(variable: LuaNode, defaultValue: LuaNode): LuaNode =
  result = luaIfStmt(
    luaElseIfBranch(
      luaInfix(
        luaIdent(lokEqualsEquals.toString),
        variable,
        luaNilLit(),
      ),
      defaultValue,
    ),
  )
  result = result.convertToExpression(variable.strVal)

proc formalParamsDefaultValueInit(s: var NimProgramState, n: NimNode): LuaNode =
  result = luaStmtList()

  for identDef in n.formalParamsIdentDefs:
    let defaultValue = identDef.identDefsValue

    if defaultValue.kind != nnkEmpty:
      for variable in identDef.identDefsVars:
        result.add(luaDefaultValueInit(s.toLuaNode(variable), s.toLuaNode(defaultValue)))

template newScope(code: untyped): untyped =
  var savedVarTypeStrs = s.varTypeStrs
  var savedProcReturnTypeStrs = s.procReturnTypeStrs
  code
  s.varTypeStrs = savedVarTypeStrs
  s.procReturnTypeStrs = savedProcReturnTypeStrs

proc callNameResolvedStr(s: var NimProgramState, n: NimNode): string =
  result = n.callName.strVal

  for callValue in n.callValues:
    result.add("_")
    if callValue.kind == nnkCall:
      result.add(s.procReturnTypeStrs[s.callNameResolvedStr(callValue)])
    else:
      result.add(s.typeStr(callValue))

proc saveIdentDefsVarTypes(s: var NimProgramState, n: NimNode) =
  for variable in n.identDefsVars:
    let typeStr = block:
      if n.identDefsType.kind != nnkEmpty:
        s.typeStr(n.identDefsType)
      else:
        s.typeStr(n.identDefsValue)

    s.varTypeStrs[variable.strVal] = typeStr

proc typeStr(s: var NimProgramState, n: NimNode): string =
  case n.kind:
  of nnkIntLit: result = "int"
  of nnkFloatLit: result = "float"
  of nnkStrLit: result = "string"
  of nnkIdent, nnkSym:
    case n.strVal:
    of "true", "false": result = "bool"
    of "int", "float", "string": result = n.strVal
    else: result = s.varTypeStrs[n.strVal]
  of nnkHiddenStdConv:
    if n[1].kind == nnkIntLit: result ="float"
    else: result = "UNKNOWN_TYPE"
  of nnkConv: result = n[0].strVal
  of nnkCall: result = s.procReturnTypeStrs[s.callNameResolvedStr(n)]
  of nnkIfExpr: result = s.typeStr(n[0].lastNode)
  of nnkCaseStmt: result = s.typeStr(n.caseStmtBranches[0].lastNode)
  of nnkStmtListExpr, nnkBlockExpr:
    newScope:
      for statement in n:
        if statement.kind in [nnkLetSection, nnkVarSection, nnkConstSection]:
          for identDef in statement:
            s.saveIdentDefsVarTypes(identDef)
      result = s.typeStr(n.lastNode)
  else: raise newException(IOError, "Tried to get type string of non type: " & $n.kind)

proc identDefsTypeStrs(s: var NimProgramState, n: NimNode): seq[string] =
  for _ in n.identDefsVars:
    if n.identDefsType.kind != nnkEmpty:
      result.add(s.typeStr(n.identDefsType))
    else:
      result.add(s.typeStr(n.identDefsValue))

proc formalParamsIdentDefsTypeStrs(s: var NimProgramState, n: NimNode): seq[string] =
  for identDef in n.formalParamsIdentDefs:
    for typeStr in s.identDefsTypeStrs(identDef):
      result.add(typeStr)

proc procDefNameResolvedStr(s: var NimProgramState, n: NimNode): string =
  result = n.procDefName.strVal

  for typeStr in s.formalParamsIdentDefsTypeStrs(n.procDefFormalParams):
    result.add("_" & typeStr)

proc convertToExpression(n: LuaNode, varName: string): LuaNode =
  var expression = n

  case expression.kind:
  of lnkStmtList, lnkElseIfBranch, lnkElseBranch:
    let lastId = expression.len - 1
    if lastId >= 0:
      expression[lastId] = expression[lastId].convertToExpression(varName)
  of lnkIfStmt:
    for i, branch in expression:
      expression[i] = branch.convertToExpression(varName)
  of lnkDoStmt:
    expression = luaDoStmt(expression[0].convertToExpression(varName))
  else:
    expression = luaAsgn(luaIdent(varName), n)

  result = expression

proc specialExprToLuaNode(s: var NimProgramState, n: NimNode, varName: string): LuaNode =
  result = s.toLuaNode(n)
  result = result.convertToExpression(varName)

proc exprAsgn(s: var NimProgramState, n: NimNode, varName: string): LuaNode =
  if n.kind in SpecialExprs:
    s.specialExprToLuaNode(n, varName)
  else:
    luaAsgn(luaIdent(varName), s.toLuaNode(n))

######################################################################

proc luaTree*(s: var NimProgramState, kind: LuaNodeKind, childNodes: varargs[LuaNode]): LuaNode =
  luaTree(kind, childNodes)

proc luaTree*(s: var NimProgramState, kind: LuaNodeKind, childNodes: varargs[NimNode]): LuaNode =
  result = luaTree(kind)
  for child in childNodes:
    result.add(s.toLuaNode(child))

proc nnkEmptyToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
  luaEmpty()

proc nnkSymToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
  luaIdent(n.strVal)

proc nnkIdentToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
  luaIdent(n.strVal)

proc nnkIntLitToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
  luaIntLit(n.intVal.int)

proc nnkFloatLitToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
  luaFloatLit(n.floatVal.float)

proc nnkStrLitToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
  luaStrLit(n.strVal)

proc nnkStmtListToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
  result = luaStmtList()
  for child in n:
    result.add(s.toLuaNode(child))

# proc nnkStmtListExprToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   newScope:
#     result = luaDoStmt(s.nnkStmtListToLuaNode(n))

proc nnkIncludeStmtToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
  luaEmpty()

proc nnkLetSectionToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
  let wasInFormalParams = s.isInFormalParams
  s.isInFormalParams = false

  result = luaStmtList()
  for identDef in n:
    result.add(luaLocal(s.toLuaNode(identDef)))

  s.isInFormalParams = wasInFormalParams

# proc nnkVarSectionToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   s.nnkLetSectionToLuaNode(n)

# proc nnkInfixToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   if n.infixOp.strVal in AsgnShortcutOperators:
#     s.exprAsgn(n.infixRight, n.infixLeft.strVal)
#   else:
#     luaInfix(
#       s.toLuaNode(n.infixOp),
#       s.toLuaNode(n.infixLeft),
#       s.toLuaNode(n.infixRight),
#     )

# proc nnkAsgnToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   s.exprAsgn(n[1], n[0].strVal)

# proc nnkIdentDefsToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   s.saveIdentDefsVarTypes(n)

#   if s.isInFormalParams:
#     result = luaFnParams()
#     for variable in n.identDefsVars:
#       result.add(s.toLuaNode(variable))

#   else:
#     result = luaStmtList()
#     let value = n.identDefsValue

#     for variable in n.identDefsVars:
#       case value.kind:
#       of nnkEmpty:
#         result.add(s.toLuaNode(variable))
#       of SpecialExprs:
#         result.add(s.toLuaNode(variable))
#         result.add(s.specialExprToLuaNode(value, variable.strVal))
#       else:
#         result.add(luaAsgn(s.toLuaNode(variable), s.toLuaNode(value)))

# proc nnkFormalParamsToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   let wasInFormalParams = s.isInFormalParams
#   s.isInFormalParams = true

#   result = luaFnParams()
#   for identDef in n.formalParamsIdentDefs:
#     result.add(s.toLuaNode(identDef))

#   s.isInFormalParams = wasInFormalParams

# proc nnkProcDefToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   var procDefNameStr = s.procDefNameResolvedStr(n)
#   s.procReturnTypeStrs[procDefNameStr] = n.procDefFormalParams.formalParamsReturnType.strVal

#   newScope:
#     var
#       functionParams = s.toLuaNode(n.procDefFormalParams)
#       functionBody = luaStmtList()

#     let defaultValueInit = s.formalParamsDefaultValueInit(n.procDefFormalParams)
#     if defaultValueInit.len > 0:
#       functionBody.add(defaultValueInit)

#     let resultName = n.procDefResultName
#     if resultName.isSome:
#       functionBody.add(luaLocal(s.toLuaNode(resultName.get)))
#       functionBody.add(luaDoStmt(s.toLuaNode(n.procDefBody)))
#       functionBody.add(luaReturnStmt(s.toLuaNode(resultName.get)))

#     else:
#       functionBody.add(s.toLuaNode(n.procDefBody))

#     result = luaLocal(luaAsgn(
#       luaIdent(procDefNameStr),
#       luaFnDef(functionParams, functionBody),
#     ))

# proc nnkBlockStmtToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   newScope:
#     result = luaDoStmt(s.toLuaNode(n[1]))

# proc nnkBlockExprToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   s.nnkBlockStmtToLuaNode(n)

# proc nnkDiscardStmtToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   s.toLuaNode(n[0])

# proc nnkCallToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   result = luaStmtList()

#   let functionNameStr = s.callNameResolvedStr(n)
#   var functionArgNames: seq[string]

#   for i, callValue in n.callValues:
#     let argNameStr = functionNameStr & "_arg_" & $i
#     functionArgNames.add(argNameStr)
#     result.add(luaLocal(luaIdent(argNameStr)))
#     result.add(s.exprAsgn(callValue, argNameStr))

#   var functionCall = luaCall(luaIdent(functionNameStr))
#   for argName in functionArgNames:
#     functionCall.add(luaIdent(argName))

#   result.add(functionCall)

# proc nnkHiddenStdConvToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   s.toLuaNode(n[1])

# proc nnkConvToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   let toType = n[0].strVal
#   case toType:
#   of "int", "float", "bool":
#     let fromType = s.typeStr(n[1])

#     if fromType == "int" and toType == "bool" or
#        fromType == "float" and toType == "bool" or
#        fromType == "float" and toType == "int" or
#        fromType == "bool" and toType == "int" or
#        fromType == "bool" and toType == "float":
#       result = luaCall(luaIdent(fromType & "_to_" & toType), s.toLuaNode(n[1]))
#     else:
#       result = s.toLuaNode(n[1])
#   else:
#     result = s.toLuaNode(n[1])

# proc nnkIfStmtToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   result = luaIfStmt()
#   for child in n:
#     result.add(s.toLuaNode(child))

# proc nnkIfExprToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   s.nnkIfStmtToLuaNode(n)

# proc nnkElifBranchToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   result = luaElseIfBranch()
#   newScope:
#     for child in n:
#       result.add(s.toLuaNode(child))

# proc nnkElifExprToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   s.nnkElifBranchToLuaNode(n)

# proc nnkElseToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   result = luaElseBranch()
#   newScope:
#     result.add(s.toLuaNode(n[0]))

# proc nnkElseExprToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   s.nnkElseToLuaNode(n)

# proc nnkCaseStmtToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   result = luaIfStmt()
#   for branch in n.caseStmtBranches:
#     newScope:
#       if branch.kind == nnkOfBranch:
#         result.add(luaElseIfBranch(
#           luaInfix(
#             luaIdent(lokEqualsEquals.toString),
#             s.toLuaNode(n.caseStmtSelector),
#             s.toLuaNode(branch[0]),
#           ),
#           s.toLuaNode(branch[1]),
#         ))
#       else:
#         result.add(s.toLuaNode(branch))

# proc nnkCommandToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   s.nnkCallToLuaNode(n)

# proc nnkConstSectionToLuaNode(s: var NimProgramState, n: NimNode): LuaNode =
#   luaEmpty()

######################################################################

macro defineToLuaNode(): untyped =
  result = nnkProcDef.newTree(
    nnkPostfix.newTree(ident("*"), ident("toLuaNode")),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      ident("LuaNode"),
      nnkIdentDefs.newTree(
        ident("s"),
        nnkVarTy.newTree(ident("NimProgramState")),
        newEmptyNode(),
      ),
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
          ident("s"),
          ident($kind & "ToLuaNode"),
        ),
        ident("n"),
      ),
    ))

  let errorAst = quote do:
    raise newException(IOError, "Unsupported NimNode kind: " & $n.kind)

  cases.add(nnkElse.newTree(errorAst))

  result[6].add(cases)

defineToLuaNode()

macro writeLua*(indentationSpaces: static[int], code: typed): untyped =
  echo code.treeRepr

  var state = NimProgramState()

  let luaCode = readFile("src/luapreamble.lua") & "\n" & state.toLuaNode(code).toLua(indentationSpaces)

  result = newStmtList(newStrLitNode(luaCode))