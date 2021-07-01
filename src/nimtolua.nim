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

proc nnkStmtListToLuaNode(n: NimNode): LuaNode =
  result = luaStmtList()
  for child in n:
    result.add(child)

proc nnkLetSectionToLuaNode(n: NimNode): LuaNode =
  let wasInFormalParams = s.isInFormalParams
  s.isInFormalParams = false

  result = luaStmtList()
  for identDef in n:
    result.add(luaLocal(identDef))

  s.isInFormalParams = wasInFormalParams

proc nnkIdentDefsToLuaNode(n: NimNode): LuaNode =
  n.saveIdentDefsVarTypes()

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

converter toLuaNode(n: NimNode): LuaNode =
  case n.kind
  of nnkEmpty, nnkIncludeStmt, nnkConstSection: luaEmpty()
  of nnkIdent, nnkSym: luaIdent(n.strVal)
  of nnkCharLit..nnkUInt64Lit: luaIntLit(n.intVal.int)
  of nnkFloatLit..nnkFloat64Lit: luaFloatLit(n.floatVal.float)
  of nnkStrLit..nnkTripleStrLit: luaStrLit(n.strVal)
  of nnkDiscardStmt: n[0]
  of nnkStmtList: n.nnkStmtListToLuaNode()
  of nnkLetSection, nnkVarSection: n.nnkLetSectionToLuaNode()
  of nnkIdentDefs: n.nnkIdentDefsToLuaNode()
  of nnkConv: n.nnkConvToLuaNode()
  else: raise newException(IOError, "Unsupported NimNode kind: " & $n.kind)

macro writeLua*(indentationSpaces: static[int], code: typed): untyped =
  echo code.treeRepr
  let luaCode = readFile("src/luapreamble.lua") & "\n" & code.toLuaNode.toLua(indentationSpaces)
  result = newStmtList(newStrLitNode(luaCode))