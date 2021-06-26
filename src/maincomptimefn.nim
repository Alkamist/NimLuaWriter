import std/macros

proc makeIndentationStr(indentation: int): string =
  for _ in 0 ..< indentation:
    result.add(" ")

proc writeLua(indentation: int, code: NimNode): string {.compileTime.} =
  let indentationStr = makeIndentationStr(indentation)
  var indentationLevel = 0

  template indent(output, input: untyped): untyped =
    for _ in 0 ..< indentationLevel:
      output.add(indentationStr)
    output.add(input)

  proc toLua(n: NimNode): string

  proc stmtListToLua(n: NimNode): string =
    for statement in n:
      result.indent(statement.toLua & "\n")

  proc bracketToLua(n: NimNode): string =
    result.add("{")
    let lastId = n.len - 1
    for i in 0 .. lastId:
      result.add(n[i].toLua)
      if i < lastId:
        result.add(", ")
    result.add("}")

  proc intLitToLua(n: NimNode): string =
    $n.intVal

  proc floatLitToLua(n: NimNode): string =
    $n.floatVal

  proc strLitToLua(n: NimNode): string =
    "\"" & n.strVal & "\""

  proc symToLua(n: NimNode): string =
    n.strVal

  proc identToLua(n: NimNode): string =
    n.strVal

  proc asgnToLua(n: NimNode): string =
    n[0].toLua & " = " & n[1].toLua

  proc identDefsToLua(n: NimNode): string =
    let lastId = n.len - 3
    for i in 0 .. lastId:
      result.add(n[i].toLua)
      if i < lastId:
        result.add(", ")

    let assignment = n[n.len - 1]
    if assignment.kind != nnkEmpty:
      result.add(" = " & assignment.toLua)

  proc infixToLua(n: NimNode): string =
    n[1].toLua & " " & n[0].toLua & " " & n[2].toLua

  proc letSectionToLua(n: NimNode): string =
    let lastId = n.len - 1
    for i in 0 .. lastId:
      result.add("local " & n[i].toLua)
      if i < lastId:
        result.add("\n")

  proc formalParamsToLua(n: NimNode): string =
    proc paramDefsToLua(n: NimNode): string =
      let lastId = n.len - 3
      for i in 0 .. lastId:
        result.add(n[i].toLua)
        if i < lastId:
          result.add(", ")

    let lastId = n.len - 1
    for i in 1 .. lastId:
      result.add(n[i].paramDefsToLua)
      if i < lastId:
        result.add(", ")

  proc returnStmtToLua(n: NimNode): string =
    "return " & n[0].toLua

  proc discardStmtToLua(n: NimNode): string =
    n[0].toLua

  proc procDefToLua(n: NimNode): string =
    if n[0].kind == nnkPostfix:
      result.add("function " & n[0][1].toLua)
    else:
      result.add("local function " & n[0].toLua)

    let params = n[3]
    result.add("(" & params.toLua & ")\n")

    var body = nnkStmtList.newTree(n[6])

    indentationLevel += 1

    result.indent("local result\n")

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
          let identValue = identDefs[identId].toLua
          result.indent("if " & identValue & " == nil then " &
                        identValue & " = " & defaultValue.toLua & " end\n")

    result.add(body.toLua)
    result.indent("return result\n")

    indentationLevel -= 1

    result.indent("end")

  proc callToLua(n: NimNode): string =
    let lastId = n.len - 1
    result.add(n[0].toLua & "(")
    for i in 1 .. lastId:
      result.add(n[i].toLua)
      if i < lastId:
        result.add(", ")
    result.add(")")

  proc toLua(n: NimNode): string =
    case n.kind:
    of nnkEmpty: ""
    of nnkTemplateDef: ""
    of nnkMacroDef: ""
    of nnkIncludeStmt: ""
    of nnkStmtList: n.stmtListToLua
    of nnkStmtListExpr: n.stmtListToLua
    of nnkBracket: n.bracketToLua
    of nnkIntLit: n.intLitToLua
    of nnkFloatLit: n.floatLitToLua
    of nnkStrLit: n.strLitToLua
    of nnkSym: n.symToLua
    of nnkIdent: n.identToLua
    of nnkAsgn: n.asgnToLua
    of nnkIdentDefs: n.identDefsToLua
    of nnkInfix: n.infixToLua
    of nnkLetSection: n.letSectionToLua
    of nnkVarSection: n.letSectionToLua
    of nnkFormalParams: n.formalParamsToLua
    of nnkReturnStmt: n.returnStmtToLua
    of nnkDiscardStmt: n.discardStmtToLua
    of nnkProcDef: n.procDefToLua
    of nnkCall: n.callToLua
    else: raise newException(IOError, "Unhandled NimNode kind: " & $n.kind)

  #echo code.treeRepr
  return code.toLua

const luaCode = writeLua(2, parseStmt(readFile("test.nim")))

echo luaCode