import std/options, general_ast, indentation

proc luaCode*(n: Node): string

proc luaCode*(n: Symbol): string =
  n.name

proc luaCode*(n: Literal): string =
  case n.kind:
  of LiteralKind.Nil: "nil"
  of LiteralKind.Char..LiteralKind.UInt64: $n.intValue
  of LiteralKind.Float..LiteralKind.Float128: $n.floatValue
  of LiteralKind.Bool: $n.boolValue
  of LiteralKind.String: "\"" & n.stringValue & "\""

proc luaCode*(n: StatementBlock): string =
  let lastId = n.nodes.len
  for i, node in n.nodes:
    result.add node.luaCode
    if i < lastId:
      result.add "\n"

proc luaCode*(n: IfBlock): string =
  for i, branch in n.elifBranches:
    if i == 0: result.add "if "
    else: result.add "elseif "

    result.add branch.condition.luaCode & " then" & IndentLevelUp &
               branch.body.luaCode & IndentLevelDown

  if n.elseBranch.isSome:
    let branch = n.elseBranch.get
    result.add "else " & IndentLevelUp &
               branch.body.luaCode & IndentLevelDown

proc luaCode*(n: Node): string =
  case n.kind:
  of NodeKind.Symbol: n.symbol.luaCode
  of NodeKind.Literal: n.literal.luaCode
  of NodeKind.StatementBlock: n.statementBlock.luaCode
  of NodeKind.IfBlock: n.ifBlock.luaCode
  else: raise newException(IOError, "Unsupported node kind for luaCode: " & $n.kind)