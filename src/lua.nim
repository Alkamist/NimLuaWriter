type
  LuaNodeKind* = enum
    lnkNone, lnkEmpty, lnkIdent,
    lnkIntLit, lnkFloatLit, lnkStrLit,
    lnkNilLit, lnkInfix, lnkPrefix, lnkStmtList,
    lnkCall, lnkLocal, lnkFnParams, lnkFnDef,
    lnkDotExpr, lnkReturnStmt, lnkBreakStmt,
    lnkDoStmt, lnkBracketExpr, lnkTableDef,
    lnkForStmt, lnkWhileStmt, lnkElseIfBranch,
    lnkElseBranch, lnkIfStmt

  LuaNode* = ref object
    case kind*: LuaNodeKind
    of lnkNone, lnkEmpty, lnkNilLit: discard
    of lnkIntLit: intVal*: BiggestInt
    of lnkFloatLit: floatVal*: BiggestFloat
    of lnkStrLit, lnkIdent: strVal*: string
    else: childNodes*: seq[LuaNode]

  LuaOperatorKind* = enum
    lokPlus = "+", lokMinus = "-", lokStar = "*", lokSlash = "/",
    lokMod = "%", lokCaret = "^",
    lokEquals = "=", lokNotEquals = "~=", lokEqualsEquals = "==",
    lokGreater = ">", lokGreaterEquals = ">=",
    lokLesser = "<", lokLesserEquals = "<=",
    lokAnd = "and", lokOr = "or", lokNot = "not",
    lokLength = "#", lokConcat = "..",

const
  IndentLevelUp = "@INDENT_UP"
  IndentLevelDown = "@INDENT_DOWN"

proc toString*(kind: LuaOperatorKind): string =
  $kind

proc newLuaNode*(kind: LuaNodeKind): LuaNode =
  LuaNode(kind: kind)

proc luaEmpty*(): LuaNode =
  newLuaNode(lnkEmpty)

proc luaNilLit*(): LuaNode =
  newLuaNode(lnkNilLit)

proc luaIdent*(strVal: string): LuaNode =
  result = newLuaNode(lnkIdent)
  result.strVal = strVal

proc luaStrLit*(strVal: string): LuaNode =
  result = newLuaNode(lnkStrLit)
  result.strVal = strVal

proc luaIntLit*(intVal: int): LuaNode =
  result = newLuaNode(lnkIntLit)
  result.intVal = intVal

proc luaFloatLit*(floatVal: float): LuaNode =
  result = newLuaNode(lnkFloatLit)
  result.floatVal = floatVal

proc luaTree*(kind: LuaNodeKind, childNodes: varargs[LuaNode]): LuaNode =
  result = newLuaNode(kind)
  if kind notin {lnkNone..lnkNilLit}:
    for child in childNodes:
      result.childNodes.add(child)

proc luaStmtList*(childNodes: varargs[LuaNode]): LuaNode =
  lnkStmtList.luaTree(childNodes)

proc luaInfix*(childNodes: varargs[LuaNode]): LuaNode =
  lnkInfix.luaTree(childNodes)

proc luaPrefix*(childNodes: varargs[LuaNode]): LuaNode =
  lnkPrefix.luaTree(childNodes)

proc luaAsgn*(name, value: LuaNode): LuaNode =
  luaInfix(
    luaIdent(lokEquals.toString),
    name,
    value,
  )

proc luaLocal*(childNodes: varargs[LuaNode]): LuaNode =
  lnkLocal.luaTree(childNodes)

proc luaDoStmt*(childNodes: varargs[LuaNode]): LuaNode =
  lnkDoStmt.luaTree(childNodes)

proc luaReturnStmt*(childNodes: varargs[LuaNode]): LuaNode =
  lnkReturnStmt.luaTree(childNodes)

proc luaFnParams*(childNodes: varargs[LuaNode]): LuaNode =
  lnkFnParams.luaTree(childNodes)

proc luaFnDef*(childNodes: varargs[LuaNode]): LuaNode =
  lnkFnDef.luaTree(childNodes)

proc luaCall*(childNodes: varargs[LuaNode]): LuaNode =
  lnkCall.luaTree(childNodes)

proc luaIfStmt*(childNodes: varargs[LuaNode]): LuaNode =
  lnkIfStmt.luaTree(childNodes)

proc luaElseIfBranch*(childNodes: varargs[LuaNode]): LuaNode =
  lnkElseIfBranch.luaTree(childNodes)

proc luaElseBranch*(childNodes: varargs[LuaNode]): LuaNode =
  lnkElseBranch.luaTree(childNodes)

proc luaTableDef*(childNodes: varargs[LuaNode]): LuaNode =
  lnkTableDef.luaTree(childNodes)

proc luaDotExpr*(childNodes: varargs[LuaNode]): LuaNode =
  lnkDotExpr.luaTree(childNodes)

proc len*(n: LuaNode): int =
  n.childNodes.len

proc `[]`*(n: LuaNode, i: int): LuaNode =
  n.childNodes[i]

proc `[]`*(n: LuaNode, i: BackwardsIndex): LuaNode =
  n.childNodes[n.len - i.int]

proc `[]=`*(n: LuaNode, i: int, child: LuaNode) =
  n.childNodes[i] = child

proc `[]=`*(n: LuaNode, i: BackwardsIndex, child: LuaNode): LuaNode =
  n.childNodes[n.len - i.int] = child

proc add*(n: LuaNode, child: LuaNode) =
  n.childNodes.add(child)

iterator items*(n: LuaNode): LuaNode {.inline.} =
  for i in 0 ..< n.len:
    yield n[i]

iterator pairs*(n: LuaNode): (int, LuaNode) {.inline.} =
  for i in 0 ..< n.len:
    yield (i, n[i])

iterator children*(n: LuaNode): LuaNode {.inline.} =
  for i in 0 ..< n.len:
    yield n[i]

proc luaStmtListIsEmpty(n: LuaNode): bool =
  if n.len < 1:
    return true

  for child in n:
    if child.kind != lnkEmpty:
      return false

proc cleanLuaNode(n: LuaNode): LuaNode =
  if n.kind in {lnkNone..lnkNilLit}:
    result = n
  else:
    result = n.kind.luaTree()
    for child in n:
      if n.kind == lnkStmtList and child.kind == lnkEmpty or
         child.kind == lnkStmtList and child.luaStmtListIsEmpty():
        continue
      result.add child.cleanLuaNode()

proc toLuaStr(n: LuaNode): string =
  case n.kind:
  of lnkEmpty: result = "EMPTY"
  of lnkNilLit: result = "nil"
  of lnkIntLit: result = $n.intVal
  of lnkFloatLit: result = $n.floatVal
  of lnkStrLit: result = "\"" & n.strVal & "\""
  of lnkIdent: result = n.strVal
  of lnkInfix: result = n[1].toLuaStr & " " & n[0].toLuaStr & " " & n[2].toLuaStr
  of lnkPrefix: result = n[0].toLuaStr & " " & n[1].toLuaStr
  of lnkLocal: result = "local " & n[0].toLuaStr
  of lnkDotExpr: result = n[0].toLuaStr & "." & n[1].toLuaStr
  of lnkReturnStmt: result = "return " & n[0].toLuaStr
  of lnkBreakStmt: result = "break"
  of lnkDoStmt: result = "do" & IndentLevelUp & n[0].toLuaStr & IndentLevelDown & "end"
  of lnkBracketExpr: result = n[0].toLuaStr & "[" & n[1].toLuaStr & "]"
  of lnkElseIfBranch: result = n[0].toLuaStr & " then" & IndentLevelUp & n[1].toLuaStr & IndentLevelDown
  of lnkElseBranch: result = IndentLevelUp & n[0].toLuaStr & IndentLevelDown

  of lnkStmtList:
    for i in 0 ..< n.len:
      result.add n[i].toLuaStr
      if i < n.len - 1:
        result.add "\n"

  of lnkCall:
    result.add n[0].toLuaStr & "("
    for i in 1 ..< n.len:
      result.add n[i].toLuaStr
      if i < n.len - 1:
        result.add ", "
    result.add ")"

  of lnkFnParams:
    for i in 0 ..< n.len:
      result.add n[i].toLuaStr
      if i < n.len - 1:
        result.add ", "

  of lnkFnDef:
    result = "function(" & n[0].toLuaStr & ")" & IndentLevelUp &
             n[1].toLuaStr & IndentLevelDown &
             "end"

  of lnkTableDef:
    let length = n.len
    result.add "{"
    if length > 0: result.add IndentLevelUp
    for i in 0 ..< length:
      result.add n[i].toLuaStr
      if i < length - 1:
        result.add ",\n"
    if length > 0: result.add IndentLevelDown
    result.add "}"

  of lnkForStmt:
    result.add "for "
    result.add n[0].toLuaStr & ", "
    result.add n[1].toLuaStr
    if n[2].kind != lnkEmpty:
      result.add ", " & n[2].toLuaStr
    result.add " do" & IndentLevelUp
    result.add n[3].toLuaStr
    result.add IndentLevelDown & "end"

  of lnkWhileStmt:
    result = "while " & n[0].toLuaStr & " do" & IndentLevelUp &
             n[1].toLuaStr &
             IndentLevelDown & "end"

  of lnkIfStmt:
    for i in 0 ..< n.len:
      if n[i].kind == lnkElseIfBranch:
        if i == 0:
          result.add "if "
        else:
          result.add "elseif "
      else:
        result.add "else "
      result.add n[i].toLuaStr
    result.add "end"

  else:
    raise newException(IOError, "Tried to convert invalid Lua node to string.")

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
    let charsTilEof = text.len - i

    if charsTilEof >= IndentLevelUp.len and
       text[i ..< i + IndentLevelUp.len] == IndentLevelUp:
      indentationLevel += 1
      i += IndentLevelUp.len
      result.add("\n")
      indent()

    elif charsTilEof >= IndentLevelDown.len and
         text[i ..< i + IndentLevelDown.len] == IndentLevelDown:
      indentationLevel -= 1
      i += IndentLevelDown.len
      result.add("\n")
      indent()

    else:
      if i > 0 and text[i - 1] == '\n':
        indent()

      result.add(text[i])
      i += 1

proc toLua*(n: LuaNode, indentationSpaces: int): string =
  addIndentation(indentationSpaces, n.cleanLuaNode().toLuaStr())