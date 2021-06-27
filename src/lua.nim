import std/macros

proc toLua(n: NimNode): string

const
  ScopeBegin = "@SCOPE_BEGIN"
  ScopeEnd = "@SCOPE_END"

#=================== Helpers ===================

proc addIndentation(spaces: int, text: string): string =
  result = ""

  var
    indentationStr = ""
    indentationLevel = 0

  for _ in 0 ..< spaces:
    indentationStr.add(" ")

  template indent(): untyped =
    for _ in 0..<indentationLevel:
      result.add(indentationStr)

  var i = 0
  while i < text.len:
    let charsTilEof = text.len - i - 1

    if charsTilEof > ScopeBegin.len and
       text[i ..< i + ScopeBegin.len] == ScopeBegin:
      indentationLevel += 1

      if i > 0 and text[i - 1] == '\n':
        indent()

      i += ScopeBegin.len

    elif charsTilEof > ScopeEnd.len and
         text[i ..< i + ScopeEnd.len] == ScopeEnd:
      indentationLevel -= 1

      if i > 0 and text[i - 1] == '\n':
        indent()

      i += ScopeEnd.len

    else:
      if i > 0 and text[i - 1] == '\n':
        indent()

      result.add(text[i])
      i += 1

proc luaTypeDefaultValue(name: string): string =
  case name:
  of "": ""
  of "int": "0"
  of "float": "0.0"
  of "bool": "false"
  of "string": ""
  else: name & "_init()"

template luaDefaultValueCode(name, value: string): string =
  "if " & name & " == nil then " & name & " = " & value & " end\n"

template separatedList(firstId, lastId, textGen, separator: untyped): untyped =
  for i in firstId .. lastId:
    result.add(textGen(i))
    if i < lastId:
      result.add(separator)

proc formalParamsDefs(n: NimNode): string =
  template generator(i: untyped): untyped = n[i].toLua
  separatedList(0, n.len - 3, generator, ", ")

proc procDefDefaultValueAssigments(n: NimNode): string =
  let
    params = n[3]
    lastParamId = params.len - 1
  for paramId in 1 .. lastParamId:
    let
      identDefs = params[paramId]
      defaultValue = identDefs[identDefs.len - 1]
    if defaultValue.kind != nnkEmpty:
      let lastIdentId = identDefs.len - 3
      for identId in 0 .. lastIdentId:
        result.add(luaDefaultValueCode(identDefs[identId].toLua, defaultValue.toLua))

proc procDefHeader(n: NimNode): string =
  let
    header = n[0]
    params = n[3]

  if header.kind == nnkPostfix:
    result.add("local function " & header[1].toLua)
  else:
    result.add("local function " & header.toLua)

  result.add("(" & params.toLua & ")\n")

proc procDefResultDef(n: NimNode): string =
  let returnType = n[3][0]
  if returnType.toLua != "":
    result.add("local result = " & luaTypeDefaultValue(returnType.toLua) & "\n")
  else:
    result.add("local result\n")

proc procDefResultReturn(n: NimNode): string =
  let body = n[6]
  if body.kind == nnkStmtList:
    let lastId = body.len - 1
    if body[lastId].kind != nnkReturnStmt:
      result.add("return result\n")

#=================== NimNodes ===================

proc stmtListToLua(n: NimNode): string =
  template generator(i: untyped): untyped = n[i].toLua
  separatedList(0, n.len - 1, generator, "\n")

proc intLitToLua(n: NimNode): string =
  $n.intVal

proc floatLitToLua(n: NimNode): string =
  $n.floatVal

proc strLitToLua(n: NimNode): string =
  "\"" & n.strVal & "\""

proc identToLua(n: NimNode): string =
  n.strVal

proc symToLua(n: NimNode): string =
  n.strVal

proc identDefsToLua(n: NimNode): string =
  template generator(i: untyped): untyped = n[i].toLua
  separatedList(0, n.len - 3, generator, ", ")

  let assignment = n[n.len - 1]
  if assignment.kind != nnkEmpty:
    result.add(" = " & assignment.toLua)

proc infixToLua(n: NimNode): string =
  n[1].toLua & " " & n[0].toLua & " " & n[2].toLua

proc asgnToLua(n: NimNode): string =
  if n[1].kind == nnkStmtListExpr:
    let
      variableName = n[0].toLua
      expression = n[1]
      expressionLastIndex = expression.len - 1

    result.add("local " & variableName & "\ndo\n" & ScopeBegin)

    template generator(i: untyped): untyped = expression[i].toLua
    separatedList(0, expressionLastIndex - 1, generator, "\n")

    result.add("\n" & variableName & " = " &
               expression[expressionLastIndex].toLua & "\n" & ScopeEnd & "end")

  else:
    result.add(n[0].toLua & " = " & n[1].toLua)

proc letSectionToLua(n: NimNode): string =
  template generator(i: untyped): untyped = "local " & n[i].toLua
  separatedList(0, n.len - 1, generator, "\n")

proc formalParamsToLua(n: NimNode): string =
  template generator(i: untyped): untyped = n[i].formalParamsDefs
  separatedList(1, n.len - 1, generator, ", ")

proc procDefToLua(n: NimNode): string =
  result.add(n.procDefHeader)
  result.add(ScopeBegin)
  result.add(n.procDefResultDef)
  result.add(n.procDefDefaultValueAssigments)
  result.add(n[6].toLua & "\n")
  result.add(n.procDefResultReturn)
  result.add(ScopeEnd)
  result.add("end")

proc returnStmtToLua(n: NimNode): string =
  if n[0].kind == nnkAsgn:
    n[0][0].toLua & " = " & n[0][1].toLua & "\nreturn " & n[0][0].toLua
  else:
    "return " & n[0].toLua

# proc discardStmtToLua(s: var LuaState, n: NimNode): string =
#   s.toLua(n[0])

# proc callToLua(s: var LuaState, n: NimNode): string =
#   result.add(s.toLua(n[0]) & "(")

#   template generator(i: untyped): untyped =
#     s.toLua(n[i])

#   separatedList(1, n.len - 1, generator, ", ")

#   result.add(")")

# proc ifStmtToLua(s: var LuaState, n: NimNode): string =
#   let lastId = n.len - 1
#   for i in 0 .. lastId:
#     if i == 0 and n[i].kind == nnkElifBranch:
#       result.add("if " & s.toLua(n[i][0]) & " then\n" &
#                  ScopeBegin & s.toLua(n[i][1]) & "\n" &
#                  ScopeEnd)
#     elif n[i].kind == nnkElifBranch:
#       result.add("elseif " & s.toLua(n[i][0]) & " then\n" &
#                  ScopeBegin & s.toLua(n[i][1]) & "\n" &
#                  ScopeEnd)
#     else:
#       result.add("else\n" &
#                  ScopeBegin & s.toLua(n[i][0]) & "\n" & ScopeEnd)

#   result.add("end")

# proc elifBranchToLua(s: var LuaState, n: NimNode): string =
#   "if "

# proc bracketToLua(s: var LuaState, n: NimNode): string =
#   result.add("{")

#   template generator(i: untyped): untyped =
#     s.toLua(n[i])

#   separatedList(0, n.len - 1, generator, ", ")

#   result.add("}")

# proc bracketExprToLua(s: var LuaState, n: NimNode): string =
#   s.toLua(n[0]) & "[" & s.toLua(n[1]) & "]"

# proc hiddenStdConvToLua(s: var LuaState, n: NimNode): string =
#   s.toLua(n[1])

# proc typeSectionToLua(s: var LuaState, n: NimNode): string =
#   for typeDef in n:
#     result.add(s.toLua(typeDef))

# proc recListIdentDefsToLua(s: var LuaState, n: NimNode): string =
#   result.add(s.toLua(n[0]))

#   template generator(i: untyped): untyped =
#     s.toLua(n[i])

#   separatedList(0, n.len - 3, generator, "\n")

# proc recListToLua(s: var LuaState, n: NimNode): string =
#   for identDef in n:
#     template generator(i: untyped): untyped =
#       s.recListIdentDefsToLua(identDef[i])

#     separatedList(0, n.len - 1, generator, "\n")

# proc typeDefToLua(s: var LuaState, n: NimNode): string =
#   let typeName = s.toLua(n[0])
#   let recList = n[2][2]

#   result.add("function " & typeName & "_init()\n")

#   result.add(s.toLua(recList))

#   result.add("end")

# proc objConstrToLua(s: var LuaState, n: NimNode): string =
#   ""
#   # s.toLua(n[0]) & ".init()"

proc toLua(n: NimNode): string =
  case n.kind:
  of nnkEmpty: ""
  of nnkTemplateDef: ""
  of nnkMacroDef: ""
  of nnkIncludeStmt: ""
  of nnkStmtList: stmtListToLua(n)
  of nnkStmtListExpr: stmtListToLua(n)
  of nnkIntLit: intLitToLua(n)
  of nnkFloatLit: floatLitToLua(n)
  of nnkStrLit: strLitToLua(n)
  of nnkIdent: identToLua(n)
  of nnkSym: symToLua(n)
  of nnkIdentDefs: identDefsToLua(n)
  of nnkInfix: infixToLua(n)
  of nnkAsgn: asgnToLua(n)
  of nnkLetSection: letSectionToLua(n)
  of nnkVarSection: letSectionToLua(n)
  of nnkFormalParams: formalParamsToLua(n)
  of nnkProcDef: procDefToLua(n)
  of nnkReturnStmt: returnStmtToLua(n)
  # of nnkDiscardStmt: s.discardStmtToLua(n)
  # of nnkCall: s.callToLua(n)
  # of nnkIfStmt: s.ifStmtToLua(n)
  # of nnkElifBranch: s.elifBranchToLua(n)
  # of nnkElse: s.elifBranchToLua(n)
  # of nnkBracket: s.bracketToLua(n)
  # of nnkBracketExpr: s.bracketExprToLua(n)
  # of nnkHiddenStdConv: s.hiddenStdConvToLua(n)
  # of nnkTypeSection: s.typeSectionToLua(n)
  # of nnkRecList: s.recListToLua(n)
  # of nnkTypeDef: s.typeDefToLua(n)
  # of nnkObjConstr: s.objConstrToLua(n)
  else: raise newException(IOError, "Unhandled NimNode kind: " & $n.kind)

macro writeLua*(indentationSpaces: static[int], code: typed): untyped =
  let luaCode = addIndentation(indentationSpaces, code.toLua)
  echo code.treeRepr
  result = newStmtList(newStrLitNode(luaCode))