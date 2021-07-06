import indentation

type
  LuaNodeKind* = enum
    None, Empty, Ident,
    IntLit, FloatLit, StrLit,
    NilLit, Infix, Prefix, StmtList,
    Call, Local, FnParams, FnDef,
    DotExpr, ReturnStmt, BreakStmt,
    DoStmt, BracketExpr, TableDef,
    ForStmt, WhileStmt, ElseIfBranch,
    ElseBranch, IfStmt,

  LuaNode* = ref object
    case kind*: LuaNodeKind
    of None, Empty, NilLit: discard
    of IntLit: intVal*: BiggestInt
    of FloatLit: floatVal*: BiggestFloat
    of StrLit, Ident: strVal*: string
    else: childNodes*: seq[LuaNode]

proc luaEmpty*(): LuaNode =
  LuaNode(kind: Empty)

proc luaNilLit*(): LuaNode =
  LuaNode(kind: NilLit)

proc luaIdent*(strVal: string): LuaNode =
  result = LuaNode(kind: Ident)
  result.strVal = strVal

proc luaStrLit*(strVal: string): LuaNode =
  result = LuaNode(kind: StrLit)
  result.strVal = strVal

proc luaIntLit*(intVal: int): LuaNode =
  result = LuaNode(kind: IntLit)
  result.intVal = intVal

proc luaFloatLit*(floatVal: float): LuaNode =
  result = LuaNode(kind: FloatLit)
  result.floatVal = floatVal

proc luaTree*(kind: LuaNodeKind, childNodes: varargs[LuaNode]): LuaNode =
  result = LuaNode(kind: kind)
  if kind notin {None..NilLit}:
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

proc toString*(n: LuaNode): string =
  case n.kind:
  of Empty: result = "EMPTY"
  of NilLit: result = "nil"
  of IntLit: result = $n.intVal
  of FloatLit: result = $n.floatVal
  of StrLit: result = "\"" & n.strVal & "\""
  of Ident: result = n.strVal
  of Infix: result = n[1].toString() & " " & n[0].toString() & " " & n[2].toString()
  of Prefix: result = n[0].toString() & " " & n[1].toString()
  of Local: result = "local " & n[0].toString()
  of DotExpr: result = n[0].toString() & "." & n[1].toString()
  of ReturnStmt: result = "return " & n[0].toString()
  of BreakStmt: result = "break"
  of DoStmt: result = "do" & IndentLevelUp & n[0].toString() & IndentLevelDown & "end"
  of BracketExpr: result = n[0].toString() & "[" & n[1].toString() & "]"
  of ElseIfBranch: result = n[0].toString() & " then" & IndentLevelUp & n[1].toString() & IndentLevelDown
  of ElseBranch: result = IndentLevelUp & n[0].toString() & IndentLevelDown

  of StmtList:
    for i in 0 ..< n.len:
      result.add n[i].toString()
      if i < n.len - 1:
        result.add "\n"

  of Call:
    result.add n[0].toString() & "("
    for i in 1 ..< n.len:
      result.add n[i].toString()
      if i < n.len - 1:
        result.add ", "
    result.add ")"

  of FnParams:
    for i in 0 ..< n.len:
      result.add n[i].toString()
      if i < n.len - 1:
        result.add ", "

  of FnDef:
    result = "function(" & n[0].toString() & ")" & IndentLevelUp &
             n[1].toString() & IndentLevelDown &
             "end"

  of TableDef:
    let length = n.len
    result.add "{"
    if length > 0: result.add IndentLevelUp
    for i in 0 ..< length:
      result.add n[i].toString()
      if i < length - 1:
        result.add ",\n"
    if length > 0: result.add IndentLevelDown
    result.add "}"

  of ForStmt:
    result.add "for "
    result.add n[0].toString() & ", "
    result.add n[1].toString()
    if n[2].kind != Empty:
      result.add ", " & n[2].toString()
    result.add " do" & IndentLevelUp
    result.add n[3].toString()
    result.add IndentLevelDown & "end"

  of WhileStmt:
    result = "while " & n[0].toString() & " do" & IndentLevelUp &
             n[1].toString() &
             IndentLevelDown & "end"

  of IfStmt:
    for i in 0 ..< n.len:
      if n[i].kind == ElseIfBranch:
        if i == 0:
          result.add "if "
        else:
          result.add "elseif "
      else:
        result.add "else "
      result.add n[i].toString()
    result.add "end"

  else:
    raise newException(IOError, "Tried to convert invalid Lua node to string.")