import
  std/[macros, tables, sequtils],
  lua,
  nimnodeshortcuts

proc unpackValues[K, V](t: Table[K, V]): seq[V] =
  for value in t.values:
    result.add(value)

proc unpackValues[K, V](t: Table[K, V], keySet: set[K]): seq[V] =
  for key in keySet:
    result.add(t[key])

const
  SpecialExprs = {nnkCaseStmt, nnkIfExpr, nnkBlockExpr, nnkStmtListExpr}
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
  NimTypeStrs = NimLiteralsToStrs.unpackValues.deduplicate
  IntTypeStrs = NimLiteralsToStrs.unpackValues({nnkIntLit..nnkInt64Lit})
  UIntTypeStrs = NimLiteralsToStrs.unpackValues({nnkUIntLit..nnkUInt64Lit})
  FloatTypeStrs = NimLiteralsToStrs.unpackValues({nnkFloatLit..nnkFloat128Lit})
  BoolTypeStr = "bool"
  BoolValues = ["true", "false"]

type
  NimProgramState = object
    isInFormalParams: bool
    procReturnTypeStrs: Table[string, string]
    varTypeStrs: Table[string, string]

var s {.compileTime.} = NimProgramState()

converter toLuaNode(n: NimNode): LuaNode
proc saveIdentDefsVarTypes(n: NimNode)
proc callNameResolvedStr(n: NimNode): string

template newScope(code: untyped): untyped =
  var savedVarTypeStrs = s.varTypeStrs
  var savedProcReturnTypeStrs = s.procReturnTypeStrs
  code
  s.varTypeStrs = savedVarTypeStrs
  s.procReturnTypeStrs = savedProcReturnTypeStrs

proc toLuaOperator(n: NimNode): LuaNode =
  luaIdent(
    case n.strVal:
    of "!=": lokNotEquals.toString
    else: n.strVal
  )

proc nimTypeStr(n: NimNode): string =
  case n.kind:
  of nnkCharLit..nnkNilLit: result = NimLiteralsToStrs[n.kind]
  of nnkIdent, nnkSym:
    case n.strVal:
    of BoolValues: result = BoolTypeStr
    of NimTypeStrs: result = n.strVal
    else: result = s.varTypeStrs[n.strVal]
  of nnkHiddenStdConv:
    if n[1].kind == nnkIntLit: result = NimLiteralsToStrs[nnkFloatLit]
    else: result = "UNKNOWN_TYPE"
  of nnkConv: result = n[0].strVal
  of nnkInfix: result = n[1].nimTypeStr
  of nnkCall: result = s.procReturnTypeStrs[n.callNameResolvedStr]
  of nnkIfExpr: result = n[0].lastNode.nimTypeStr
  of nnkCaseStmt: result = n.caseStmtBranches[0].lastNode.nimTypeStr
  of nnkStmtListExpr, nnkBlockExpr:
    newScope:
      for statement in n:
        if statement.kind in [nnkLetSection, nnkVarSection, nnkConstSection]:
          for identDef in statement:
            identDef.saveIdentDefsVarTypes()
      result = n.lastNode.nimTypeStr
  else: raise newException(IOError, "Tried to get type string of non type: " & $n.kind)

# proc convertToExpression(n: LuaNode; varName: string, infixOp = none(string)): LuaNode =
#   var expression = n

#   case expression.kind:
#   of lnkStmtList, lnkElseIfBranch, lnkElseBranch:
#     let lastId = expression.len - 1
#     if lastId >= 0:
#       expression[lastId] = expression[lastId].convertToExpression(varName, infixOp)
#   of lnkIfStmt:
#     for i, branch in expression:
#       expression[i] = branch.convertToExpression(varName, infixOp)
#   of lnkDoStmt:
#     expression = luaDoStmt(expression[0].convertToExpression(varName, infixOp))
#   else:
#     if infixOp.isSome:
#       expression = luaAsgn(
#         luaIdent(varName),
#         luaInfix(
#           luaIdent(infixOp.get),
#           luaIdent(varName),
#           n,
#         ),
#       )
#     else:
#       expression = luaAsgn(luaIdent(varName), n)

#   result = expression

proc nnkCaseStmtToIfStmt(n: NimNode): NimNode =
  result = nnkIfStmt.newTree()
  for branch in n.caseStmtBranches:
    newScope:
      if branch.kind == nnkOfBranch:
        result.add(nnkElifBranch.newTree(
          nnkInfix.newTree(
            ident("=="),
            n.caseStmtSelector,
            branch[0],
          ),
          branch[1],
        ))
      else:
        result.add(branch)

proc convertToExpression(n: NimNode; varName: string): NimNode =
  var expression = n

  case expression.kind:
  of nnkStmtList, nnkStmtListExpr,
     nnkElifBranch, nnkElifExpr,
     nnkElse, nnkElseExpr:
    let lastId = expression.len - 1
    if lastId >= 0:
      expression[lastId] = expression[lastId].convertToExpression(varName)
  of nnkIfStmt, nnkIfExpr:
    for i, branch in expression:
      expression[i] = branch.convertToExpression(varName)
  of nnkCaseStmt:
    expression = expression.nnkCaseStmtToIfStmt.convertToExpression(varName)
  else:
    expression = nnkAsgn.newTree(ident(varName), n)

  result = expression

proc saveIdentDefsVarTypes(n: NimNode) =
  for variable in n.identDefsVars:
    let typeStr = block:
      if n.identDefsType.kind != nnkEmpty:
        n.identDefsType.nimTypeStr
      else:
        n.identDefsValue.nimTypeStr

    s.varTypeStrs[variable.strVal] = typeStr

proc callNameResolvedStr(n: NimNode): string =
  result = n.callName.strVal

  for callValue in n.callValues:
    result.add("_")
    if callValue.kind == nnkCall:
      result.add(s.procReturnTypeStrs[callValue.callNameResolvedStr])
    else:
      result.add(callValue.nimTypeStr)

proc identDefsTypeStrs(n: NimNode): seq[string] =
  for _ in n.identDefsVars:
    if n.identDefsType.kind != nnkEmpty:
      result.add(n.identDefsType.nimTypeStr)
    else:
      result.add(n.identDefsValue.nimTypeStr)

proc formalParamsIdentDefsTypeStrs(n: NimNode): seq[string] =
  for identDefs in n.formalParamsIdentDefs:
    for typeStr in identDefs.identDefsTypeStrs:
      result.add(typeStr)

proc procDefNameResolvedStr(n: NimNode): string =
  result = n.procDefName.strVal
  for typeStr in n.procDefFormalParams.formalParamsIdentDefsTypeStrs:
    result.add("_" & typeStr)

# proc luaDefaultValueInit(variable: LuaNode, defaultValue: LuaNode): LuaNode =
#   result = luaIfStmt(
#     luaElseIfBranch(
#       luaInfix(
#         luaIdent(lokEqualsEquals.toString),
#         variable,
#         luaNilLit(),
#       ),
#       defaultValue,
#     ),
#   )
#   result = result.convertToExpression(variable.strVal)

# proc formalParamsDefaultValueInit(n: NimNode): LuaNode =
#   result = luaStmtList()

#   for identDefs in n.formalParamsIdentDefs:
#     let defaultValue = identDefs.identDefsValue

#     if defaultValue.kind != nnkEmpty:
#       for variable in identDefs.identDefsVars:
#         result.add(luaDefaultValueInit(variable, defaultValue))

proc nnkStmtListToLuaNode(n: NimNode): LuaNode =
  result = luaStmtList()
  for child in n:
    result.add(child)

proc nnkStmtListExprToLuaNode(n: NimNode): LuaNode =
  newScope:
    result = luaDoStmt(n.nnkStmtListToLuaNode)

proc nnkBlockStmtToLuaNode(n: NimNode): LuaNode =
  newScope:
    result = luaDoStmt(n[1])

proc nnkIfStmtToLuaNode(n: NimNode): LuaNode =
  result = luaIfStmt()
  for child in n:
    result.add(child)

proc nnkElifBranchToLuaNode(n: NimNode): LuaNode =
  result = luaElseIfBranch()
  newScope:
    for child in n:
      result.add(child)

proc nnkElseToLuaNode(n: NimNode): LuaNode =
  result = luaElseBranch()
  newScope:
    result.add(n[0])

proc nnkCaseStmtToLuaNode(n: NimNode): LuaNode =
  n.nnkCaseStmtToIfStmt

proc nnkLetSectionToLuaNode(n: NimNode): LuaNode =
  let wasInFormalParams = s.isInFormalParams
  s.isInFormalParams = false

  result = luaStmtList()
  for identDefs in n:
    identDefs.saveIdentDefsVarTypes()

    let value = identDefs.identDefsValue

    case value.kind:
    of SpecialExprs:
      for variable in identDefs.identDefsVars:
        result.add(luaLocal(variable))
        result.add(value.convertToExpression(variable.strVal))
    # of nnkInfix:
    #   for variable in identDefs.identDefsVars:
    #     result.add(luaLocal(variable))
    #     result.add(value.infixLeft.convertToExpression(variable.strVal))
    #     result.add(value.infixRight.convertToExpression(variable.strVal, some(value.infixOp.strVal)))
    else:
      result.add(luaLocal(identDefs))

  s.isInFormalParams = wasInFormalParams

proc nnkIdentDefsToLuaNode(n: NimNode): LuaNode =
  if s.isInFormalParams:
    result = luaFnParams()
    for variable in n.identDefsVars:
      result.add(variable)

  else:
    result = luaStmtList()
    let value = n.identDefsValue

    for variable in n.identDefsVars:
      case value.kind:
      of nnkEmpty: result.add(variable)
      else: result.add(luaAsgn(variable, value))

proc nnkConvToLuaNode(n: NimNode): LuaNode =
  let toType = n[0].strVal

  case toType:
  of IntTypeStrs, UIntTypeStrs, FloatTypeStrs, BoolTypeStr:
    let fromType = n[1].nimTypeStr

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

proc nnkInfixToLuaNode(n: NimNode): LuaNode =
  luaInfix(
    n.infixOp.toLuaOperator,
    n.infixLeft,
    n.infixRight,
  )

proc nnkAsgnToLuaNode(n: NimNode): LuaNode =
  luaAsgn(n[0], n[1])

# proc nnkFormalParamsToLuaNode(n: NimNode): LuaNode =
#   let wasInFormalParams = s.isInFormalParams
#   s.isInFormalParams = true

#   result = luaFnParams()
#   for identDef in n.formalParamsIdentDefs:
#     result.add(identDef)

#   s.isInFormalParams = wasInFormalParams

# proc nnkProcDefToLuaNode(n: NimNode): LuaNode =
#   var procDefNameStr = n.procDefNameResolvedStr
#   s.procReturnTypeStrs[procDefNameStr] = n.procDefFormalParams.formalParamsReturnType.strVal

#   newScope:
#     var
#       functionParams = n.procDefFormalParams.toLuaNode
#       functionBody = luaStmtList()

#     let defaultValueInit = n.procDefFormalParams.formalParamsDefaultValueInit
#     if defaultValueInit.len > 0:
#       functionBody.add(defaultValueInit)

#     let resultName = n.procDefResultName
#     if resultName.isSome:
#       functionBody.add(luaLocal(resultName.get))
#       functionBody.add(luaDoStmt(n.procDefBody))
#       functionBody.add(luaReturnStmt(resultName.get))

#     else:
#       functionBody.add(n.procDefBody)

#     result = luaLocal(luaAsgn(
#       luaIdent(procDefNameStr),
#       luaFnDef(functionParams, functionBody),
#     ))

# proc nnkCallToLuaNode(n: NimNode): LuaNode =
#   result = luaStmtList()

#   let functionNameStr = n.callNameResolvedStr
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

converter toLuaNode(n: NimNode): LuaNode =
  case n.kind
  of nnkEmpty, nnkIncludeStmt, nnkConstSection: luaEmpty()
  of nnkIdent, nnkSym: luaIdent(n.strVal)
  of nnkCharLit..nnkUInt64Lit: luaIntLit(n.intVal.int)
  of nnkFloatLit..nnkFloat64Lit: luaFloatLit(n.floatVal.float)
  of nnkStrLit..nnkTripleStrLit: luaStrLit(n.strVal)
  of nnkDiscardStmt: n[0]
  of nnkStmtList: n.nnkStmtListToLuaNode
  of nnkStmtListExpr: n.nnkStmtListExprToLuaNode
  of nnkBlockStmt, nnkBlockExpr: n.nnkBlockStmtToLuaNode
  of nnkIfStmt, nnkIfExpr: n.nnkIfStmtToLuaNode
  of nnkElifBranch, nnkElifExpr: n.nnkElifBranchToLuaNode
  of nnkElse, nnkElseExpr: n.nnkElseToLuaNode
  of nnkCaseStmt: n.nnkCaseStmtToLuaNode
  of nnkLetSection, nnkVarSection: n.nnkLetSectionToLuaNode
  of nnkIdentDefs: n.nnkIdentDefsToLuaNode
  of nnkConv: n.nnkConvToLuaNode
  of nnkHiddenStdConv: n[1]
  of nnkInfix: n.nnkInfixToLuaNode
  of nnkAsgn: n.nnkAsgnToLuaNode
  # of nnkFormalParams: n.nnkFormalParamsToLuaNode
  # of nnkProcDef: n.nnkProcDefToLuaNode
  # of nnkCall, nnkCommand: n.nnkCallToLuaNode()
  else: raise newException(IOError, "Unsupported NimNode kind: " & $n.kind)

macro writeLua*(indentationSpaces: static[int], code: typed): untyped =
  echo code.treeRepr
  let luaCode = readFile("src/luapreamble.lua") & "\n" & code.toLuaNode.toLua(indentationSpaces)
  result = newStmtList(newStrLitNode(luaCode))