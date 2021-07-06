import luaast

proc luaStmtListIsEmpty(n: LuaNode): bool =
  if n.len < 1:
    return true

  for child in n:
    if child.kind != Empty:
      return false

proc cleanLuaNode(n: LuaNode): LuaNode =
  if n.kind in {None..NilLit}:
    result = n
  else:
    result = n.kind.luaTree()
    for child in n:
      if n.kind == StmtList and child.kind == Empty or
         child.kind == StmtList and child.luaStmtListIsEmpty():
        continue
      result.add child.cleanLuaNode()

proc toLua*(n: LuaNode, indentationSpaces: int): string =
  addIndentation(indentationSpaces, n.cleanLuaNode().toLuaStr())