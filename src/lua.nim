import
  std/macros,
  std/tables

type
  LuaNodeKind* = enum
    lnkNone, lnkEmpty, lnkIdent, lnkBoolLit,
    lnkIntLit, lnkFloatLit, lnkStrLit,
    lnkNilLit, lnkInfix, lnkStmtList

  LuaNode* = ref object
    case kind*: LuaNodeKind
    of lnkNone, lnkEmpty, lnkNilLit: discard
    of lnkBoolLit: boolVal*: bool
    of lnkIntLit: intVal*: BiggestInt
    of lnkFloatLit: floatVal*: BiggestFloat
    of lnkStrLit, lnkIdent: strVal*: string
    else: childNodes*: seq[LuaNode]

const lnkNonTrees = {lnkNone..lnkNilLit}

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

proc newLuaNilLit*(): LuaNode =
  newLuaNode(lnkNilLit)

proc newLuaBoolLit*(boolVal: bool): LuaNode =
  result = newLuaNode(lnkBoolLit)
  result.boolVal = boolVal

proc newLuaIdent*(strVal: string): LuaNode =
  result = newLuaNode(lnkIdent)
  result.strVal = strVal

proc newLuaStrLit*(strVal: string): LuaNode =
  result = newLuaNode(lnkStrLit)
  result.strVal = strVal

proc newLuaIntLit*(intVal: int): LuaNode =
  result = newLuaNode(lnkIntLit)
  result.intVal = intVal

proc newLuaFloatLit*(floatVal: float): LuaNode =
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
  for i, child in n:
    result.add(child.toLua)
    if i < n.len - 1:
      result.add("\n")

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

# dumptree:
#   let a = 5

when isMainModule:
  var test = lnkStmtList.newLuaTree(
    lnkInfix.newLuaTree(newLuaIdent("+"), newLuaIdent("a"), newLuaIdent("b")),
    lnkInfix.newLuaTree(newLuaIdent("+"), newLuaIdent("a"), newLuaIdent("b")),
  )
  echo test.toLua