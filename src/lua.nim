import
  std/macros,
  std/tables

type
  LuaNodeKind* = enum
    lnkNone, lnkEmpty, lnkIdent, lnkBoolLit,
    lnkIntLit, lnkFloatLit, lnkStrLit,
    lnkNilLit, lnkInfix, lnkStmtList,
    lnkCall, lnkLocal, lnkFnParams, lnkFnDef,
    lnkDotExpr, lnkReturnStmt

  LuaNode* = ref object
    case kind*: LuaNodeKind
    of lnkNone, lnkEmpty, lnkNilLit: discard
    of lnkBoolLit: boolVal*: bool
    of lnkIntLit: intVal*: BiggestInt
    of lnkFloatLit: floatVal*: BiggestFloat
    of lnkStrLit, lnkIdent: strVal*: string
    else: childNodes*: seq[LuaNode]

const
  lnkNonTrees = {lnkNone..lnkNilLit}
  ScopeBegin = "@SCOPE_BEGIN"
  ScopeEnd = "@SCOPE_END"

proc toLua*(n: LuaNode): string

proc newLuaNode*(kind: LuaNodeKind): LuaNode =
  LuaNode(kind: kind)

proc newLuaTree*(kind: LuaNodeKind, childNodes: varargs[LuaNode]): LuaNode =
  case kind:
  of lnkNonTrees:
    raise newException(IOError, "Invalid LuaNodeKind for newTree: " & $kind)
  else:
    result = newLuaNode(kind)
    for child in childNodes:
      result.childNodes.add(child)

proc newLuaEmptyNode*(): LuaNode =
  newLuaNode(lnkEmpty)

proc newLuaNilLitNode*(): LuaNode =
  newLuaNode(lnkNilLit)

proc newLuaBoolLitNode*(boolVal: bool): LuaNode =
  result = newLuaNode(lnkBoolLit)
  result.boolVal = boolVal

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

proc len*(n: LuaNode): int =
  n.childNodes.len

proc `[]`*(n: LuaNode, i: int): LuaNode =
  n.childNodes[i]

proc `[]`*(n: LuaNode, i: BackwardsIndex): LuaNode =
  n.childNodes[n.len - i.int]

proc `[]=`*(n: var LuaNode, i: int, v: LuaNode) =
  n.childNodes[i] = v

proc `[]=`*(n: var LuaNode, i: BackwardsIndex, v: LuaNode): LuaNode =
  n.childNodes[n.len - i.int] = v

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

proc lnkInfixToLua*(n: LuaNode): string =
 n[1].toLua & " " & n[0].toLua & " " & n[2].toLua

proc lnkStmtListToLua*(n: LuaNode): string =
  for i in 0..<n.len:
    result.add(n[i].toLua)
    if i < n.len - 1:
      result.add("\n")

proc lnkCallToLua*(n: LuaNode): string =
  result.add(n[0].toLua & "(")
  for i in 1..<n.len:
    result.add(n[i].toLua)
    if i < n.len - 1:
      result.add(", ")
  result.add(")")

proc lnkLocalToLua*(n: LuaNode): string =
  "local " & n.toLua

proc lnkFnParamsToLua*(n: LuaNode): string =
  result.add("(")
  for i in 0..<n.len:
    result.add(n[i].toLua)
    if i < n.len - 1:
      result.add(", ")
  result.add(")")

proc lnkFnDefToLua*(n: LuaNode): string =
  "function " & n[0].toLua & n[1].toLua & ScopeBegin &
  n[2].toLua & ScopeEnd &
  "end"

proc lnkDotExprToLua*(n: LuaNode): string =
  n[0].toLua & "." & n[1].toLua

proc lnkReturnStmtToLua*(n: LuaNode): string =
  "return " & n[0].toLua

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

  var cases = quote do:
    case n.kind:
    of lnkNone: raise newException(IOError, "Tried to convert invalid Lua node to string.")
    of lnkEmpty: ""
    of lnkNilLit: "nil"
    of lnkBoolLit: $n.boolVal
    of lnkIntLit: $n.intVal
    of lnkFloatLit: $n.floatVal
    of lnkStrLit, lnkIdent: n.strVal

  for kind in LuaNodeKind:
    if kind notin lnkNonTrees:
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
    let charsTilEof = text.len - i - 1

    if charsTilEof > ScopeBegin.len and
       text[i ..< i + ScopeBegin.len] == ScopeBegin:
      indentationLevel += 1
      i += ScopeBegin.len
      result.add("\n")
      indent()

    elif charsTilEof > ScopeEnd.len and
         text[i ..< i + ScopeEnd.len] == ScopeEnd:
      indentationLevel -= 1
      i += ScopeEnd.len
      result.add("\n")
      indent()

    else:
      if i > 0 and text[i - 1] == '\n':
        indent()

      result.add(text[i])
      i += 1

proc toLua*(n: LuaNode, indentationSpaces: int): string =
  addIndentation(indentationSpaces, n.toLua)

######################################################################

when isMainModule:
  var test = lnkFnDef.newLuaTree(
    newLuaIdentNode("testFn"),
    lnkFnParams.newLuaTree(
      newLuaIdentNode("a"),
      newLuaIdentNode("b"),
    ),
    lnkDotExpr.newLuaTree(
      newLuaIdentNode("z"),
      lnkCall.newLuaTree(
        newLuaIdentNode("add"),
        lnkInfix.newLuaTree(newLuaIdentNode("+"), newLuaIdentNode("a"), newLuaIdentNode("b")),
        lnkInfix.newLuaTree(newLuaIdentNode("+"), newLuaIdentNode("a"), newLuaIdentNode("b")),
      ),
    ),
  )
  echo test.toLua(2)