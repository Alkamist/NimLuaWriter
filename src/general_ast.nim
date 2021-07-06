import std/options, indentation

type
  TypeKind* {.pure.} = enum
    Char,
    Int, Int8, Int16, Int32, Int64,
    UInt, UInt8, UInt16, UInt32, UInt64,
    Float, Float32, Float64, Float128,
    Bool, String,
    Function, Object,
    Enum, EnumField,

  Symbol* = object
    name*: string
    typeKind*: TypeKind

  LiteralKind* {.pure.} = enum
    Nil, Char,
    Int, Int8, Int16, Int32, Int64,
    UInt, UInt8, UInt16, UInt32, UInt64,
    Float, Float32, Float64, Float128,
    Bool, String,

  Literal* = object
    case kind*: LiteralKind
    of LiteralKind.Nil: discard
    of LiteralKind.Char..LiteralKind.UInt64: intValue*: BiggestInt
    of LiteralKind.Float..LiteralKind.Float128: floatValue*: BiggestFloat
    of LiteralKind.Bool: boolValue*: bool
    of LiteralKind.String: stringValue*: string

  StatementBlock* = object
    nodes*: seq[Node]
    isExpression*: bool

  ElifBranch* = object
    condition*: Node
    body*: Node

  ElseBranch* = object
    body*: Node

  IfBlock* = object
    elifBranches*: seq[ElifBranch]
    elseBranch*: Option[ElseBranch]
    isExpression*: bool

  NodeKind* {.pure.} = enum
    Symbol, Literal, StatementBlock,
    ElifBranch, ElseBranch, IfBlock,

  Node* = ref object
    case kind*: NodeKind
    of NodeKind.Symbol: symbol*: Symbol
    of NodeKind.Literal: literal*: Literal
    of NodeKind.StatementBlock: statementBlock*: StatementBlock
    of NodeKind.ElifBranch: elifBranch*: ElifBranch
    of NodeKind.ElseBranch: elseBranch*: ElseBranch
    of NodeKind.IfBlock: ifBlock*: IfBlock

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
