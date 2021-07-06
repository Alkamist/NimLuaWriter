import
  compiler/types,
  std/[tables, strutils],
  hnimast/[compiler_aux, hast_common],
  lua

const
  SpecialExprs = {nkCaseStmt, nkIfExpr, nkBlockExpr, nkStmtListExpr, nkObjConstr}
  NimOpChars = ['=', '+', '-', '*', '/', '<', '>', '@', '$', '~',
                '&', '%', '|', '!','?', '^', '.', ':', '\\']
  NimKeywordOps = ["and", "or", "not", "xor", "shl", "shr", "div", "mod",
                   "in", "notin", "is", "isnot", "of", "as", "from"]
  NimShortcutOps = ["+=", "-=", "*=", "/=", "%="]
  LuaBinaryOps = ["+", "-", "*", "/", "%", "^", "=", "~=", "==",
                  ">", ">=", "<", "<=", "and", "or", ".."]
  LuaUnaryOps = ["not", "#"]

type
  NimProgramState = object
    tempVarCount: int
    nativeFns: Table[string, string]

var s: NimProgramState

proc genTempVarName(s: var NimProgramState): string =
  result = "TEMP_" & $s.tempVarCount
  s.tempVarCount += 1

converter toLuaNode(n: PNode): LuaNode

####################################################################

proc nimOperatorCharToWord(op: char): string =
  case op:
  of '=': "equals"
  of '+': "plus"
  of '-': "minus"
  of '*': "star"
  of '/': "slash"
  of '<': "lesser"
  of '>': "greater"
  of '@': "at"
  of '$': "dollar"
  of '~': "tilde"
  of '&': "ampersand"
  of '%': "percent"
  of '|': "vertbar"
  of '!': "exclaim"
  of '?': "question"
  of '^': "caret"
  of '.': "dot"
  of ':': "colon"
  of '\\': "backslash"
  else: raise newException(IOError, "Unsupported char for nimOperatorToWord: " & op)

proc nimOperatorToFnName(op: string): string =
  result = "operator"

  if op in NimKeywordOps:
    result.add "_" & op

  else:
    for c in op:
      result.add "_" & c.nimOperatorCharToWord()

proc nkProcDefIsOperatorOverload(n: PNode): bool =
  let procNameStr = $n[0]
  procNameStr[0] in NimOpChars or procNameStr in NimKeywordOps

proc nkProcDefNameStr(n: PNode): string =
  let originalFnNameStr = $n[0]

  if n.nkProcDefIsOperatorOverload():
    result = originalFnNameStr.nimOperatorToFnName()
  else:
    result = originalFnNameStr

  for identDefs in n[3][1 ..^ 1]:
    let value = identDefs[^1]
    for varSym in identDefs[0 ..^ 3]:
      result.add "_" & ($varSym.typ).replace(" ")

proc typeIsBuiltin(n: PNode): bool =
  n.typ.kind != tyObject

proc isBuiltinBinaryOperator(n, left, right: PNode): bool =
  n.sym.kind == skProc and
  left.typeIsBuiltin() and
  right.typeIsBuiltin() and
  $n in LuaBinaryOps

proc luaDefaultValue(n: PNode): LuaNode =
  case n.typ.kind:
  of tyFloat..tyFloat128: luaFloatLit(0.0)
  of tyInt..tyInt64: luaIntLit(0)
  of tyUInt..tyUInt64: luaIntLit(0)
  of tyEnum, tyString, tyCString: luaStrLit("")
  of tyObject: luaCall(luaIdent($n.typ & "_init"))
  else: raise newException(IOError, "Unsupported type kind for default value: " & $n.typ.kind)

proc luaDefaultValueInit(variable: LuaNode, defaultValue: LuaNode): LuaNode =
  luaIfStmt(
    luaElseIfBranch(
      luaInfix(
        luaIdent(lokEqualsEquals.toString),
        variable,
        luaNilLit(),
      ),
      luaAsgn(variable, defaultValue),
    ),
  )

proc containsSpecialExpr(n: PNode): bool =
  if n.kind in SpecialExprs:
    return true
  for child in n:
    if child.containsSpecialExpr():
      return true

proc convertToExpression(n: LuaNode, varName: string): LuaNode =
  case n.kind:

  of lnkIfStmt:
    result = luaIfStmt()
    for branch in n:
      result.add branch.convertToExpression(varName)

  of lnkStmtList, lnkDoStmt, lnkElseIfBranch, lnkElseBranch:
    result = n.kind.luaTree()

    let lastId = n.len - 1
    for i, child in n:
      if i == lastId:
        result.add child.convertToExpression(varName)
      else:
        result.add child

  else:
    result = luaAsgn(luaIdent(varName), n)

proc specialExprResolution(n: PNode): (LuaNode, LuaNode) =
  if n.kind == nkInfix:
    var
      exprResolutions = luaStmtList()
      exprAssignments = luaInfix(n[0])

    let (leftResolutions, leftAssignments) = n[1].specialExprResolution()
    exprResolutions.add leftResolutions
    exprAssignments.add leftAssignments

    let (rightResolutions, rightAssignments) = n[2].specialExprResolution()
    exprResolutions.add rightResolutions
    exprAssignments.add rightAssignments

    result = (exprResolutions, exprAssignments)
  else:
    let exprSym = luaIdent(s.genTempVarName())

    var exprResolutions =
      if n.kind == nkObjConstr:
        var output = luaStmtList(
          luaLocal(luaAsgn(
            exprSym,
            luaCall(luaIdent($n.typ & "_init")),
          )),
        )

        for field in n[1 ..^ 1]:
          output.add luaAsgn(luaDotExpr(exprSym, field[0]), field[1])

        output
      else:
        luaStmtList(
          luaLocal(exprSym),
          n.convertToExpression(exprSym.strVal),
        )

    result = (exprResolutions, exprSym)

proc processNativeFnStr(n: PNode, nativeStr: string): string =
  var replaceArgId = 0

  for c in nativeStr:
    if replaceArgId < n.len - 1:
      case c:

      of '#':
        result.add $n[replaceArgId+1]
        replaceArgId += 1

        continue

      of '@':
        while replaceArgId < n.len - 1:
          result.add $n[replaceArgId+1]

          if replaceArgId < n.len - 2:
            result.add ", "

          replaceArgId += 1

        continue

      else:
        discard

    result.add c

####################################################################

proc toLuaOperator(n: PNode): LuaNode =
  luaIdent(
    case $n:
    of "+=": "+"
    of "-=": "-"
    of "*=": "*"
    of "/=": "/"
    of "%=": "%"
    of "!=": lokNotEquals.toString
    else: $n
  )

proc nkSymToLuaNode(n: PNode): LuaNode =
  if n.sym.kind == skEnumField and n.typ.kind != tyBool:
    luaStrLit($n)
  else:
    luaIdent($n)

proc nkAsgnToLuaNode(n: PNode): LuaNode =
  if n[1].containsSpecialExpr():
    let (exprResolutions, exprAssignments) = n[1].specialExprResolution()
    result = luaStmtList(
      exprResolutions,
      luaAsgn(n[0], exprAssignments),
    )
  else:
    result = luaAsgn(n[0], n[1])

proc nkInfixToLuaNode(n: PNode): LuaNode =
  let operator = n[0].toLuaOperator()

  if $n[0] in NimShortcutOps:
    if n[2].containsSpecialExpr():
      let (exprResolutions, exprAssignments) = n[2].specialExprResolution()
      result = luaStmtList(
        exprResolutions,
        luaAsgn(n[1], luaInfix(operator, n[1], exprAssignments)),
      )
    else:
      result = luaAsgn(n[1], luaInfix(operator, n[1], n[2]))
  else:
    if n[0].isBuiltinBinaryOperator(n[1], n[2]):
      result = luaInfix(operator, n[1], n[2])
    else:
      result = luaCall(luaIdent(($n[0]).nimOperatorToFnName()), n[1], n[2])

proc nkLetOrVarSectionToLuaNode(n: PNode, sectionKind: TNodeKind): LuaNode =
  result = luaStmtList()
  for identDefs in n:
    let value = identDefs[^1]
    for varSym in identDefs[0 ..^ 3]:
      if value.containsSpecialExpr():
        let (exprResolutions, exprAssignments) = value.specialExprResolution()
        result.add exprResolutions
        result.add luaLocal(luaAsgn(varSym, exprAssignments))
      else:
        if value.kind == nkEmpty:
          result.add luaLocal(varSym)
        else:
          result.add luaLocal(luaAsgn(varSym, value))

proc nkCaseStmtToLuaNode(n: PNode): LuaNode =
  result = luaIfStmt()

  for branch in n[1 ..^ 1]:
    if branch.kind == nkOfBranch:
      result.add luaElseIfBranch(
        luaInfix(luaIdent("=="), n[0], branch[0]),
        branch[1],
      )
    else:
      result.add branch

proc nkConvToLuaNode(n: PNode): LuaNode =
  const
    tyIntTypes = {tyChar, tyInt..tyInt64}
    tyUIntTypes = {tyUInt..tyUInt64}
    tyFloatTypes = {tyFloat..tyFloat128}

  let toTypeKind = n[0].typ.kind

  case toTypeKind:
  of tyIntTypes, tyUIntTypes, tyFloatTypes, tyBool:
    let fromTypeKind = n[1].typ.kind

    if fromTypeKind in tyIntTypes and toTypeKind == tyBool or
       fromTypeKind in tyUIntTypes and toTypeKind == tyBool or
       fromTypeKind in tyFloatTypes and toTypeKind == tyBool or
       fromTypeKind in tyFloatTypes and toTypeKind in tyIntTypes or
       fromTypeKind == tyBool and toTypeKind in tyIntTypes or
       fromTypeKind == tyBool and toTypeKind in tyUIntTypes or
       fromTypeKind == tyBool and toTypeKind in tyFloatTypes:
      result = luaCall(luaIdent(fromTypeKind.toHumanStr() & "_to_" & toTypeKind.toHumanStr()), n[1])
    else:
      result = n[1]
  else:
    result = n[1]

proc nkTypeDefToLuaNode(n: PNode): LuaNode =
  case n[2].typ.kind:
  of tyEnum: result = luaEmpty()
  of tyObject:
    var fnBody = luaStmtList(luaLocal(luaAsgn(luaIdent("self"), luaTableDef())))

    for identDefs in n[2][2]:
      for varSym in identDefs[0 ..^ 3]:
        fnBody.add luaAsgn(
          luaDotExpr(luaIdent("self"), varSym),
          identDefs[^2].luaDefaultValue(),
        )

    fnBody.add luaReturnStmt(luaIdent("self"))

    result = luaLocal(luaAsgn(
      luaIdent($n[0] & "_init"),
      luaFnDef(
        luaFnParams(),
        fnBody,
      ),
    ))
  else: raise newException(IOError, "Unsupported type kind for type def: " & $n[0].typ.kind)

proc nkProcDefToLuaNode(n: PNode): LuaNode =
  var
    fnNameStr = n.nkProcDefNameStr()
    fnParams = luaFnParams()
    fnParamsExprResolutions = luaStmtList()
    fnParamsDefaultValueInits = luaStmtList()

  for identDefs in n[3][1 ..^ 1]:
    let value = identDefs[^1]
    for varSym in identDefs[0 ..^ 3]:
      fnParams.add varSym

      if value.kind != nkEmpty:
        if value.containsSpecialExpr():
          let (exprResolutions, exprAssignments) = value.specialExprResolution()
          fnParamsExprResolutions.add exprResolutions
          fnParamsDefaultValueInits.add varSym.luaDefaultValueInit(exprAssignments)
        else:
          fnParamsDefaultValueInits.add varSym.luaDefaultValueInit(value)

  let fnHasResult = n[3][0].kind != nkEmpty
  var fnBody = luaStmtList(fnParamsDefaultValueInits)
  if fnHasResult:
    fnBody.add luaLocal(luaAsgn(n[7], n[3][0].luaDefaultValue()))
    fnBody.add n[6]
    fnBody.add luaReturnStmt(n[7])
  else:
    fnBody.add n[6]

  if n[4].kind == nkPragma and
     n[4][0].kind == nkExprColonExpr and
     n[4][0][0].kind == nkIdent and
     $n[4][0][0] == "importcpp":
    s.nativeFns[fnNameStr] = n[4][0][1].strVal
    result = luaEmpty()
  else:
    result = luaStmtList(
      fnParamsExprResolutions,
      luaLocal(luaAsgn(
        luaIdent(fnNameStr),
        luaFnDef(
          fnParams,
          fnBody,
        ),
      ),
    ))

proc nkCallToLuaNode(n: PNode): LuaNode =
  var callNameStr = $n[0]

  for callArg in n[1 ..^ 1]:
    callNameStr.add "_"

    let actualArg =
      if callArg.kind == nkHiddenAddr:
        callNameStr.add "var"
        callArg[0]
      else:
        callArg

    if actualArg.typ.kind == tyObject:
      callNameStr.add $actualArg.typ
    else:
      callNameStr.add actualArg.typ.kind.toHumanStr()

  if s.nativeFns.contains(callNameStr):
    result = luaIdent(n.processNativeFnStr(s.nativeFns[callNameStr]))
  else:
    var
      callNode = luaCall(luaIdent(callNameStr))
      callExprResolutions = luaStmtList()

    for callArg in n[1 ..^ 1]:
      if callArg.containsSpecialExpr():
        let (exprResolutions, exprAssignments) = callArg.specialExprResolution()
        callExprResolutions.add exprResolutions
        callNode.add exprAssignments
      else:
        callNode.add callArg

    result = luaStmtList(
      callExprResolutions,
      callNode,
    )

template unpackTo(luaTreeFn: untyped): untyped =
  result = luaTreeFn()
  for child in n:
    result.add child

proc nkStmtListToLuaNode(n: PNode): LuaNode = unpackTo(luaStmtList)
proc nkIfStmtToLuaNode(n: PNode): LuaNode = unpackTo(luaIfStmt)
proc nkElifBranchToLuaNode(n: PNode): LuaNode = unpackTo(luaElseIfBranch)
proc nkElseToLuaNode(n: PNode): LuaNode = unpackTo(luaElseBranch)
proc nkTypeSectionToLuaNode(n: PNode): LuaNode = unpackTo(luaStmtList)

converter toLuaNode(n: PNode): LuaNode =
  case n.kind
  of nkEmpty, nkIncludeStmt, nkConstSection, nkPragma: luaEmpty()
  of nkIdent: luaIdent($n)
  of nkCharLit..nkUInt64Lit: luaIntLit(n.intVal.int)
  of nkFloatLit..nkFloat64Lit: luaFloatLit(n.floatVal.float)
  of nkStrLit..nkTripleStrLit: luaStrLit(n.strVal)
  of nkDiscardStmt, nkHiddenDeref, nkHiddenAddr: n[0]
  of nkAsgn: n.nkAsgnToLuaNode()
  of nkInfix: n.nkInfixToLuaNode()
  of nkPrefix: luaPrefix(n[0].toLuaOperator(), n[1])
  of nkDotExpr: luaDotExpr(n[0], n[1])
  of nkSym: n.nkSymToLuaNode()
  of nkTypeSection: n.nkTypeSectionToLuaNode()
  of nkBlockStmt, nkBlockExpr: luaDoStmt(n.nkStmtListToLuaNode())
  of nkStmtList, nkStmtListExpr: n.nkStmtListToLuaNode()
  of nkLetSection, nkVarSection: n.nkLetOrVarSectionToLuaNode(n.kind)
  of nkCaseStmt: n.nkCaseStmtToLuaNode()
  of nkIfStmt, nkIfExpr: n.nkIfStmtToLuaNode()
  of nkElifBranch, nkElifExpr: n.nkElifBranchToLuaNode()
  of nkElse, nkElseExpr: n.nkElseToLuaNode()
  of nkConv: n.nkConvToLuaNode()
  of nkTypeDef: n.nkTypeDefToLuaNode()
  of nkProcDef: n.nkProcDefToLuaNode()
  of nkCall, nkCommand: n.nkCallToLuaNode()
  else: raise newException(IOError, "Unsupported PNode kind: " & $n.kind)

proc writeLua*(nimCode: string, indentationSpaces: int): string =
  let n = nimCode.compileString()
  echo n.treeRepr()
  n.toLuaNode().toLua(indentationSpaces)