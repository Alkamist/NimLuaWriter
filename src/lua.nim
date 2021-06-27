import std/macros


template luaTypeDefaultValue(name: string): untyped =
  case name:
  of "int": 0
  of "float": 0.0
  of "bool": false
  of "string": ""
  else: name & "_init()"

template luaDefaultValueCode(name, value: string): string =
  "if " & name & " == nil then " & name & " = " & value & " end\n"

template separatedList(firstId, lastId, textGen, separator: untyped): untyped =
  for i in firstId .. lastId:
    result.add(textGen(i))
    if i < lastId:
      result.add(separator)


type
  LuaWriter = object
    indentationLevel: int
    indentationStr: string

proc toLua(s: var LuaWriter, n: NimNode): string

proc `indentationSpaces=`(s: var LuaWriter, value: int) =
  for _ in 0 ..< value:
    s.indentationStr.add(" ")

proc indent(s: var LuaWriter, text: string): string =
  for _ in 0 ..< s.indentationLevel:
    result.add(s.indentationStr)
  result.add(text)

proc stmtListToLua(s: var LuaWriter, n: NimNode): string =
  let lastId = n.len - 1

  result.add(s.toLua(n[0]))
  if lastId > 0:
    result.add("\n")

  template generator(i: untyped): untyped =
    s.indent(s.toLua(n[i]))

  separatedList(1, n.len - 1, generator, "\n")

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

proc procDefToLua(s: var LuaWriter, n: NimNode): string =
  if n[0].kind == nnkPostfix:
    result.add("function " & s.toLua(n[0][1]))
  else:
    result.add("local function " & s.toLua(n[0]))

  let params = n[3]
  result.add("(" & s.toLua(params) & ")\n")

  var body = n[6]

  s.indentationLevel += 1

  result.add(s.indent("local result\n"))

  # Insert assignments of default values of arguments
  # to the beginning of function bodies.
  let lastParamId = params.len - 1
  for paramId in 1 .. lastParamId:
    let
      identDefs = params[paramId]
      defaultValue = identDefs[identDefs.len - 1]
    if defaultValue.kind != nnkEmpty:
      let lastIdentId = identDefs.len - 3
      for identId in 0 .. lastIdentId:
        result.add(s.indent(
          luaDefaultValueCode(s.toLua(identDefs[identId]),
                              s.toLua(defaultValue))
        ))

  result.add(s.indent(s.toLua(body) & "\n"))
  result.add(s.indent("return result\n"))

  s.indentationLevel -= 1

  result.add(s.indent("end"))

proc returnStmtToLua(s: var LuaWriter, n: NimNode): string =
  "return " & s.toLua(n[0])

proc discardStmtToLua(s: var LuaWriter, n: NimNode): string =
  s.toLua(n[0])

proc callToLua(s: var LuaWriter, n: NimNode): string =
  result.add(s.toLua(n[0]) & "(")

  template generator(i: untyped): untyped =
    s.toLua(n[i])

  separatedList(1, n.len - 1, generator, ", ")

  result.add(")")

proc bracketToLua(s: var LuaWriter, n: NimNode): string =
  result.add("{")

  template generator(i: untyped): untyped =
    s.toLua(n[i])

  separatedList(0, n.len - 1, generator, ", ")

  result.add("}")

proc bracketExprToLua(s: var LuaWriter, n: NimNode): string =
  s.toLua(n[0]) & "[" & s.toLua(n[1]) & "]"

proc hiddenStdConvToLua(s: var LuaWriter, n: NimNode): string =
  s.toLua(n[1])

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
#       s.indent(s.recListIdentDefsToLua(identDef[i]))

#     separatedList(0, n.len - 1, generator, "\n")

# proc typeDefToLua(s: var LuaWriter, n: NimNode): string =
#   let typeName = s.toLua(n[0])
#   let recList = n[2][2]

#   result.add("function " & typeName & "_init()\n")

#   result.add(s.indent(s.toLua(recList)))

#   result.add(s.indent("end"))

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
  of nnkBracket: s.bracketToLua(n)
  of nnkBracketExpr: s.bracketExprToLua(n)
  of nnkHiddenStdConv: s.hiddenStdConvToLua(n)
  # of nnkTypeSection: s.typeSectionToLua(n)
  # of nnkRecList: s.recListToLua(n)
  # of nnkTypeDef: s.typeDefToLua(n)
  # of nnkObjConstr: s.objConstrToLua(n)
  else: raise newException(IOError, "Unhandled NimNode kind: " & $n.kind)


macro writeLua*(indentation: static[int], code: typed): untyped =
  var luaWriter: LuaWriter
  luaWriter.indentationSpaces = indentation

  echo code.treeRepr

  result = newStmtList(newStrLitNode(luaWriter.toLua(code)))