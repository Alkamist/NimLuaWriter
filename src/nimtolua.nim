import
  std/macros,
  std/options,
  std/tables,
  lua

const
  SupportedNimNodeKinds = {nnkEmpty, nnkSym, nnkIdent, nnkIntLit, nnkFloatLit,
                           nnkStrLit, nnkStmtList, nnkIncludeStmt,
                           nnkLetSection, nnkVarSection,
                           nnkInfix, nnkAsgn, nnkIdentDefs, nnkFormalParams,
                           nnkProcDef, nnkBlockStmt, nnkDiscardStmt,
                           nnkHiddenStdConv, nnkCall}

type
  NimToLuaState = object
    isInFormalParams: bool
    procReturnTypeStrs: Table[string, string]

proc toLuaNode*(s: var NimToLuaState, n: NimNode): LuaNode
proc typeStr(s: var NimToLuaState, n: NimNode): string

######################################################################
# NimNode Children
######################################################################

template identDefVars(n: NimNode): untyped =
  n[0..<n.len-2]

template identDefType(n: NimNode): untyped =
  n[n.len-2]

template identDefValue(n: NimNode): untyped =
  n[n.len-1]

template objConstrValues(n: NimNode): untyped =
  n[1..<n.len]

template formalParamsIdentDefs(n: NimNode): untyped =
  n[1..<n.len]

template formalParamsReturnType(n: NimNode): untyped =
  n[0]

template procDefName(n: NimNode): untyped =
  n[0]

template procDefFormalParams(n: NimNode): untyped =
  n[3]

template procDefBody(n: NimNode): untyped =
  n[6]

proc procDefResultName(n: NimNode): Option[NimNode] =
  if n.procDefFormalParams[0].kind != nnkEmpty:
    return some(n[7])

template callName(n: NimNode): untyped =
  n[0]

template callValues(n: NimNode): untyped =
  n[1..<n.len]

######################################################################
# Helpers
######################################################################

proc callNameResolvedStr(s: var NimToLuaState, n: NimNode): string =
  result = n.callName.strVal

  for callValue in n.callValues:
    result.add("_")
    if callValue.kind == nnkCall:
      result.add(s.procReturnTypeStrs[s.callNameResolvedStr(callValue)])
    else:
      result.add(s.typeStr(callValue))

proc typeStr(s: var NimToLuaState, n: NimNode): string =
  case n.kind:
  of nnkIntLit: "int"
  of nnkFloatLit: "float"
  of nnkStrLit: "string"
  of nnkIdent, nnkSym:
    if n.strVal in ["true", "false"]: "bool"
    else: n.strVal
  of nnkHiddenStdConv:
    if n[1].kind == nnkIntLit: "float"
    else: "UNKNOWN_TYPE"
  of nnkCall: s.procReturnTypeStrs[s.callNameResolvedStr(n)]
  else: raise newException(IOError, "Tried to get type string of non type: " & $n.kind)

proc formalParamsIdentDefsTypeStrs(s: var NimToLuaState, n: NimNode): seq[string] =
  for identDef in n.formalParamsIdentDefs:
    if identDef.identDefType.kind != nnkEmpty:
      result.add(s.typeStr(identDef.identDefType))
    else:
      result.add(s.typeStr(identDef.identDefValue))

proc procDefNameResolvedStr(s: var NimToLuaState, n: NimNode): string =
  result = n.procDefName.strVal

  for typeStr in s.formalParamsIdentDefsTypeStrs(n.procDefFormalParams):
    result.add("_" & typeStr)

######################################################################
# NimNode To LuaNode
######################################################################

proc nnkEmptyToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  luaEmpty()

proc nnkSymToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  luaIdent(n.strVal)

proc nnkIdentToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  luaIdent(n.strVal)

proc nnkIntLitToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  luaIntLit(n.intVal.int)

proc nnkFloatLitToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  luaFloatLit(n.floatVal.float)

proc nnkStrLitToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  luaStrLit(n.strVal)

proc nnkStmtListToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  result = luaStmtList()
  for child in n:
    result.add(s.toLuaNode(child))

proc nnkIncludeStmtToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  luaEmpty()

proc nnkLetSectionToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  let wasInFormalParams = s.isInFormalParams
  s.isInFormalParams = false

  result = luaStmtList()
  for identDef in n:
    result.add(luaLocal(s.toLuaNode(identDef)))

  s.isInFormalParams = wasInFormalParams

proc nnkVarSectionToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  s.nnkLetSectionToLuaNode(n)

proc nnkInfixToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  result = luaInfix()
  for child in n:
    result.add(s.toLuaNode(child))

proc nnkAsgnToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  luaAsgn(s.toLuaNode(n[0]), s.toLuaNode(n[1]))

proc nnkIdentDefsToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  if s.isInFormalParams:
    result = luaFnParams()
    for variable in n.identDefVars:
      result.add(s.toLuaNode(variable))

  else:
    result = luaStmtList()
    let value = n.identDefValue

    for variable in n.identDefVars:
      result.add(luaAsgn(s.toLuaNode(variable), s.toLuaNode(value)))

proc nnkFormalParamsToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  let wasInFormalParams = s.isInFormalParams
  s.isInFormalParams = true

  result = luaFnParams()
  for identDef in n.formalParamsIdentDefs:
    result.add(s.toLuaNode(identDef))

  s.isInFormalParams = wasInFormalParams

proc nnkProcDefToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  var procDefNameStr = s.procDefNameResolvedStr(n)
  s.procReturnTypeStrs[procDefNameStr] = n.procDefFormalParams.formalParamsReturnType.strVal

  var
    functionParams = s.toLuaNode(n.procDefFormalParams)
    functionBody = luaStmtList()

  let resultName = n.procDefResultName
  if resultName.isSome:
    functionBody.add(luaLocal(s.toLuaNode(resultName.get)))
    functionBody.add(luaDoStmt(s.toLuaNode(n.procDefBody)))
    functionBody.add(luaReturnStmt(s.toLuaNode(resultName.get)))

  else:
    functionBody.add(s.toLuaNode(n.procDefBody))

  result = luaLocal(luaAsgn(
    luaIdent(procDefNameStr),
    luaFnDef(functionParams, functionBody),
  ))

proc nnkBlockStmtToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  luaDoStmt(s.toLuaNode(n[1]))

proc nnkDiscardStmtToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  s.toLuaNode(n[0])

proc nnkCallToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  result = luaCall(luaIdent(s.callNameResolvedStr(n)))
  for callValue in n.callValues:
    result.add(s.toLuaNode(callValue))

proc nnkHiddenStdConvToLuaNode(s: var NimToLuaState, n: NimNode): LuaNode =
  s.toLuaNode(n[1])

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
        nnkVarTy.newTree(ident("NimToLuaState")),
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
  var state = NimToLuaState()
  let luaCode = state.toLuaNode(code).toLua(indentationSpaces)
  result = newStmtList(newStrLitNode(luaCode))