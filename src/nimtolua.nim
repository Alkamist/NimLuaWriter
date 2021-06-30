import
  std/macros,
  std/options,
  std/tables,
  lua

const
  ScopeOpeners = {nnkBlockStmt, nnkProcDef}
  SupportedNimNodeKinds = {nnkEmpty, nnkSym, nnkIdent, nnkIntLit, nnkFloatLit,
                           nnkStrLit, nnkStmtList, nnkIncludeStmt,
                           nnkLetSection, nnkVarSection,
                           nnkInfix, nnkAsgn, nnkIdentDefs, nnkFormalParams,
                           nnkProcDef, nnkBlockStmt, nnkDiscardStmt, nnkCall}

type
  NimToLuaState = object
    isInFormalParams: bool
    procReturnTypeStrs: Table[string, string]

proc toLuaNode*(s: var NimToLuaState, n: NimNode): LuaNode

######################################################################
# NimNode Children
######################################################################

proc identDefVars(n: NimNode): seq[NimNode] =
  n[0..<n.len-2]

proc identDefType(n: NimNode): NimNode =
  n[n.len-2]

proc identDefValue(n: NimNode): NimNode =
  n[n.len-1]

proc objConstrValues(n: NimNode): seq[NimNode] =
  n[1..<n.len]

proc formalParamsIdentDefs(n: NimNode): seq[NimNode] =
  n[1..<n.len]

proc formalParamsIdentDefsTypes(n: NimNode): seq[NimNode] =
  for identDef in n.formalParamsIdentDefs:
    result.add(identDef.identDefType)

proc formalParamsReturnType(n: NimNode): NimNode =
  n[0]

proc procDefName(n: NimNode): NimNode =
  n[0]

proc procDefFormalParams(n: NimNode): NimNode =
  n[3]

proc procDefBody(n: NimNode): NimNode =
  n[6]

proc procDefResultName(n: NimNode): Option[NimNode] =
  if n.procDefFormalParams[0].kind != nnkEmpty:
    return some(n[7])

proc callName(n: NimNode): NimNode =
  n[0]

proc callValues(n: NimNode): seq[NimNode] =
  n[1..<n.len]

######################################################################
# Helpers
######################################################################

proc typeStr(n: NimNode): string =
  case n.kind:
  of nnkIntLit: "int"
  of nnkFloatLit: "float"
  of nnkStrLit: "string"
  of nnkIdent:
    if n.strVal in ["true", "false"]: "bool"
    else: n.strVal
  else: raise newException(IOError, "Tried to get type string of non type: " & $n.kind)

proc callNameStrResolved(s: var NimToLuaState, n: NimNode): string =
  result = n.callName.strVal

  for callValue in n.callValues:
    result.add("_")
    if callValue.kind == nnkCall:
      result.add(s.procReturnTypeStrs[s.callNameStrResolved(callValue)])
    else:
      result.add(callValue.typeStr)

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
  var procDefNameStr = n.procDefName.strVal

  for paramType in n.procDefFormalParams.formalParamsIdentDefsTypes:
    procDefNameStr.add("_" & paramType.strVal)

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
  result = luaCall(luaIdent(s.callNameStrResolved(n)))
  for callValue in n.callValues:
    result.add(s.toLuaNode(callValue))

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