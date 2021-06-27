import std/macros


const
  ScopeBegin = "@SCOPE_BEGIN"
  ScopeEnd = "@SCOPE_END"

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


type LuaWriter = object

proc toLua(s: var LuaWriter, n: NimNode): string

proc addIndentation(s: var LuaWriter, spaces: int, text: string): string =
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

proc stmtListToLua(s: var LuaWriter, n: NimNode): string =
  template generator(i: untyped): untyped =
    s.toLua(n[i])

  separatedList(0, n.len - 1, generator, "\n")

proc intLitToLua(s: var LuaWriter, n: NimNode): string =
  $n.intVal

proc floatLitToLua(s: var LuaWriter, n: NimNode): string =
  $n.floatVal

proc strLitToLua(s: var LuaWriter, n: NimNode): string =
  "\"" & n.strVal & "\""

proc identToLua(s: var LuaWriter, n: NimNode): string =
  n.strVal

proc symToLua(s: var LuaWriter, n: NimNode): string =
  n.strVal

proc identDefsToLua(s: var LuaWriter, n: NimNode): string =
  template generator(i: untyped): untyped =
    s.toLua(n[i])

  separatedList(0, n.len - 3, generator, ", ")

  let assignment = n[n.len - 1]
  if assignment.kind != nnkEmpty:
    result.add(" = " & s.toLua(assignment))

proc infixToLua(s: var LuaWriter, n: NimNode): string =
  s.toLua(n[1]) & " " & s.toLua(n[0]) & " " & s.toLua(n[2])

proc asgnToLua(s: var LuaWriter, n: NimNode): string =
  s.toLua(n[0]) & " = " & s.toLua(n[1])

proc letSectionToLua(s: var LuaWriter, n: NimNode): string =
  template generator(i: untyped): untyped =
    "local " & s.toLua(n[i])

  separatedList(0, n.len - 1, generator, "\n")

proc paramDefsToLua(s: var LuaWriter, n: NimNode): string =
  template generator(i: untyped): untyped =
    s.toLua(n[i])

  separatedList(0, n.len - 3, generator, ", ")

proc formalParamsToLua(s: var LuaWriter, n: NimNode): string =
  template generator(i: untyped): untyped =
    s.paramDefsToLua(n[i])

  separatedList(1, n.len - 1, generator, ", ")

proc paramDefaultValueAssigments(s: var LuaWriter, n: NimNode): string =
  let lastParamId = n.len - 1
  for paramId in 1 .. lastParamId:
    let
      identDefs = n[paramId]
      defaultValue = identDefs[identDefs.len - 1]
    if defaultValue.kind != nnkEmpty:
      let lastIdentId = identDefs.len - 3
      for identId in 0 .. lastIdentId:
        result.add(
          luaDefaultValueCode(s.toLua(identDefs[identId]),
                              s.toLua(defaultValue))
        )

proc procDefToLua(s: var LuaWriter, n: NimNode): string =
  let
    header = n[0]
    params = n[3]
    returnType = params[0]
    returnTypeStr = s.toLua(returnType)
    body = n[6]

  if header.kind == nnkPostfix:
    result.add("local function " & s.toLua(header[1]))
  else:
    result.add("local function " & s.toLua(header))

  result.add("(" & s.toLua(params) & ")\n")
  result.add(ScopeBegin)

  if returnTypeStr != "":
    result.add("local result = " & luaTypeDefaultValue(returnTypeStr) & "\n")
  else:
    result.add("local result\n")

  result.add(s.paramDefaultValueAssigments(params))

  if body.kind == nnkAsgn and
     body[0].kind == nnkSym and
     body[0].strVal == "result":
    let
      expression = body[1]
      expressionLen = expression.len

    template generator(i: untyped): untyped =
      s.toLua(expression[i])

    separatedList(0, expressionLen - 2, generator, "\n")
    result.add("\nresult = " & s.toLua(expression[expressionLen - 1]) & "\n")

  else:
    result.add(s.toLua(n[6]) & "\n")

  result.add("return result\n")
  result.add(ScopeEnd)
  result.add("end")

proc returnStmtToLua(s: var LuaWriter, n: NimNode): string =
  result.add("return ")
  if n[0].kind == nnkAsgn:
    result.add(s.toLua(n[0][1]))
  else:
    result.add(s.toLua(n[0]))

proc discardStmtToLua(s: var LuaWriter, n: NimNode): string =
  s.toLua(n[0])

proc callToLua(s: var LuaWriter, n: NimNode): string =
  result.add(s.toLua(n[0]) & "(")

  template generator(i: untyped): untyped =
    s.toLua(n[i])

  separatedList(1, n.len - 1, generator, ", ")

  result.add(")")

proc ifStmtToLua(s: var LuaWriter, n: NimNode): string =
  let lastId = n.len - 1
  for i in 0 .. lastId:
    if i == 0 and n[i].kind == nnkElifBranch:
      result.add("if " & s.toLua(n[i][0]) & " then\n" &
                 ScopeBegin & s.toLua(n[i][1]) & "\n" &
                 ScopeEnd)
    elif n[i].kind == nnkElifBranch:
      result.add("elseif " & s.toLua(n[i][0]) & " then\n" &
                 ScopeBegin & s.toLua(n[i][1]) & "\n" &
                 ScopeEnd)
    else:
      result.add("else\n" &
                 ScopeBegin & s.toLua(n[i][0]) & "\n" & ScopeEnd)

  result.add("end")

proc elifBranchToLua(s: var LuaWriter, n: NimNode): string =
  "if "

# proc bracketToLua(s: var LuaWriter, n: NimNode): string =
#   result.add("{")

#   template generator(i: untyped): untyped =
#     s.toLua(n[i])

#   separatedList(0, n.len - 1, generator, ", ")

#   result.add("}")

# proc bracketExprToLua(s: var LuaWriter, n: NimNode): string =
#   s.toLua(n[0]) & "[" & s.toLua(n[1]) & "]"

# proc hiddenStdConvToLua(s: var LuaWriter, n: NimNode): string =
#   s.toLua(n[1])

# proc typeSectionToLua(s: var LuaWriter, n: NimNode): string =
#   for typeDef in n:
#     result.add(s.toLua(typeDef))

# proc recListIdentDefsToLua(s: var LuaWriter, n: NimNode): string =
#   result.add(s.toLua(n[0]))

#   template generator(i: untyped): untyped =
#     s.toLua(n[i])

#   separatedList(0, n.len - 3, generator, "\n")

# proc recListToLua(s: var LuaWriter, n: NimNode): string =
#   for identDef in n:
#     template generator(i: untyped): untyped =
#       s.recListIdentDefsToLua(identDef[i])

#     separatedList(0, n.len - 1, generator, "\n")

# proc typeDefToLua(s: var LuaWriter, n: NimNode): string =
#   let typeName = s.toLua(n[0])
#   let recList = n[2][2]

#   result.add("function " & typeName & "_init()\n")

#   result.add(s.toLua(recList))

#   result.add("end")

# proc objConstrToLua(s: var LuaWriter, n: NimNode): string =
#   ""
#   # s.toLua(n[0]) & ".init()"

proc toLua(s: var LuaWriter, n: NimNode): string =
  case n.kind:
  of nnkEmpty: ""
  of nnkTemplateDef: ""
  of nnkMacroDef: ""
  of nnkIncludeStmt: ""
  of nnkStmtList: s.stmtListToLua(n)
  of nnkStmtListExpr: s.stmtListToLua(n)
  of nnkIntLit: s.intLitToLua(n)
  of nnkFloatLit: s.floatLitToLua(n)
  of nnkStrLit: s.strLitToLua(n)
  of nnkIdent: s.identToLua(n)
  of nnkSym: s.symToLua(n)
  of nnkIdentDefs: s.identDefsToLua(n)
  of nnkInfix: s.infixToLua(n)
  of nnkAsgn: s.asgnToLua(n)
  of nnkLetSection: s.letSectionToLua(n)
  of nnkVarSection: s.letSectionToLua(n)
  of nnkFormalParams: s.formalParamsToLua(n)
  of nnkProcDef: s.procDefToLua(n)
  of nnkReturnStmt: s.returnStmtToLua(n)
  of nnkDiscardStmt: s.discardStmtToLua(n)
  of nnkCall: s.callToLua(n)
  of nnkIfStmt: s.ifStmtToLua(n)
  of nnkElifBranch: s.elifBranchToLua(n)
  of nnkElse: s.elifBranchToLua(n)
  # of nnkBracket: s.bracketToLua(n)
  # of nnkBracketExpr: s.bracketExprToLua(n)
  # of nnkHiddenStdConv: s.hiddenStdConvToLua(n)
  # of nnkTypeSection: s.typeSectionToLua(n)
  # of nnkRecList: s.recListToLua(n)
  # of nnkTypeDef: s.typeDefToLua(n)
  # of nnkObjConstr: s.objConstrToLua(n)
  else: raise newException(IOError, "Unhandled NimNode kind: " & $n.kind)


macro writeLua*(indentationSpaces: static[int], code: typed): untyped =
  var luaWriter: LuaWriter
  let
    unindentedLuaCode = luaWriter.toLua(code)
    indentedLuaCode = luaWriter.addIndentation(indentationSpaces, unindentedLuaCode)

  echo code.treeRepr
  result = newStmtList(newStrLitNode(indentedLuaCode))