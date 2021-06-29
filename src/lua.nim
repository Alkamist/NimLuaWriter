import
  std/macros,
  std/tables,
  std/strutils

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

proc toLua*(n: LuaNode): string

proc toString*(kind: LuaOperatorKind): string =
  $kind

proc toLuaOperatorKind*(text: string): LuaOperatorKind =
  parseEnum[LuaOperatorKind](text)

proc newLuaNode*(kind: LuaNodeKind): LuaNode =
  LuaNode(kind: kind)

proc newLuaEmptyNode*(): LuaNode =
  newLuaNode(lnkEmpty)

proc newLuaNilLitNode*(): LuaNode =
  newLuaNode(lnkNilLit)

proc newLuaIdentNode*(strVal: string): LuaNode =
  result = newLuaNode(lnkIdent)
  result.strVal = strVal

proc newLuaStrLitNode*(strVal: string): LuaNode =
  result = newLuaNode(lnkStrLit)
  result.strVal = strVal

proc newLuaIntLitNode*(intVal: int): LuaNode =
  result = newLuaNode(lnkIntLit)
  result.intVal = intVal

proc newLuaFloatLitNode*(floatVal: float): LuaNode =
  result = newLuaNode(lnkFloatLit)
  result.floatVal = floatVal

proc newLuaTree*(kind: LuaNodeKind, childNodes: varargs[LuaNode]): LuaNode =
  case kind:
  of lnkNone..lnkNilLit:
    raise newException(IOError, "Invalid LuaNodeKind for newLuaTree: " & $kind)
  else:
    result = newLuaNode(kind)
    for child in childNodes:
      result.childNodes.add(child)

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

######################################################################
# Code Writing
######################################################################

proc lnkNoneToLua(n: LuaNode): string =
  raise newException(IOError, "Tried to convert invalid Lua node to string.")

proc lnkEmptyToLua(n: LuaNode): string =
  ""

proc lnkNilLitToLua(n: LuaNode): string =
  "nil"

proc lnkIntLitToLua(n: LuaNode): string =
  $n.intVal

proc lnkFloatLitToLua(n: LuaNode): string =
  $n.floatVal

proc lnkStrLitToLua(n: LuaNode): string =
  "\"" & n.strVal & "\""

proc lnkIdentToLua(n: LuaNode): string =
  n.strVal

proc lnkInfixToLua(n: LuaNode): string =
 n[1].toLua & " " & n[0].toLua & " " & n[2].toLua

proc lnkPrefixToLua(n: LuaNode): string =
 n[0].toLua & n[1].toLua

proc lnkStmtListToLua(n: LuaNode): string =
  for i in 0..<n.len:
    result.add(n[i].toLua)
    if i < n.len - 1:
      result.add("\n")

proc lnkCallToLua(n: LuaNode): string =
  result.add(n[0].toLua & "(")
  for i in 1..<n.len:
    result.add(n[i].toLua)
    if i < n.len - 1:
      result.add(", ")
  result.add(")")

proc lnkLocalToLua(n: LuaNode): string =
  "local " & n[0].toLua

proc lnkFnParamsToLua(n: LuaNode): string =
  result.add("(")
  for i in 0..<n.len:
    result.add(n[i].toLua)
    if i < n.len - 1:
      result.add(", ")
  result.add(")")

proc lnkFnDefToLua(n: LuaNode): string =
  "function" & n[0].toLua & IndentLevelUp &
  n[1].toLua & IndentLevelDown &
  "end"

proc lnkDotExprToLua(n: LuaNode): string =
  n[0].toLua & "." & n[1].toLua

proc lnkReturnStmtToLua(n: LuaNode): string =
  "return " & n[0].toLua

proc lnkBreakStmtToLua(n: LuaNode): string =
  "break"

proc lnkDoStmtToLua(n: LuaNode): string =
  "do" & IndentLevelUp & n[0].toLua & IndentLevelDown & "end"

proc lnkBracketExprToLua(n: LuaNode): string =
  n[0].toLua & "[" & n[1].toLua & "]"

proc lnkTableDefToLua(n: LuaNode): string =
  let length = n.len
  result.add("{")
  if length > 0: result.add(IndentLevelUp)
  for i in 0 ..< length:
    result.add(n[i].toLua)
    if i < length - 1:
      result.add(",\n")
  if length > 0: result.add(IndentLevelDown)
  result.add("}")

proc lnkForStmtToLua(n: LuaNode): string =
  result.add("for ")
  result.add(n[0].toLua & ", ")
  result.add(n[1].toLua)
  if n[2].kind != lnkEmpty:
    result.add(", " & n[2].toLua)
  result.add(" do" & IndentLevelUp)
  result.add(n[3].toLua)
  result.add(IndentLevelDown & "end")

proc lnkWhileStmtToLua(n: LuaNode): string =
  "while " & n[0].toLua & " do" & IndentLevelUp &
  n[1].toLua &
  IndentLevelDown & "end"

proc lnkElseIfBranchToLua(n: LuaNode): string =
  n[0].toLua & " then" & IndentLevelUp & n[1].toLua & IndentLevelDown

proc lnkElseBranchToLua(n: LuaNode): string =
  IndentLevelUp & n[0].toLua & IndentLevelDown

proc lnkIfStmtToLua(n: LuaNode): string =
  for i in 0 ..< n.len:
    if n[i].kind == lnkElseIfBranch:
      if i == 0:
        result.add("if ")
      else:
        result.add("elseif ")
    else:
      result.add("else ")
    result.add(n[i].toLua)
  result.add("end")

######################################################################

macro defineToLua(): untyped =
  result = nnkProcDef.newTree(
    nnkPostfix.newTree(ident("*"), ident("toLua")),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      ident("string"),
      nnkIdentDefs.newTree(
        ident("n"),
        ident("LuaNode"),
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

  for kind in LuaNodeKind:
    cases.add(nnkOfBranch.newTree(
      ident($kind),
      nnkCall.newTree(
        nnkDotExpr.newTree(
          ident("n"),
          ident($kind & "ToLua"),
        ),
      ),
    ))

  result[6].add(cases)

defineToLua()

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
  addIndentation(indentationSpaces, n.toLua)