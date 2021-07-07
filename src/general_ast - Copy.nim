import std/options

type
  GTypeKind* {.pure.} = enum
    Nil,
    Char,
    Int, Int8, Int16, Int32, Int64,
    UInt, UInt8, UInt16, UInt32, UInt64,
    Float, Float32, Float64, Float128,
    String,
    Object, Enum, EnumField

  GLiteralKind* {.pure.} = enum
    Nil,
    Char,
    Int, Int8, Int16, Int32, Int64,
    UInt, UInt8, UInt16, UInt32, UInt64,
    Float, Float32, Float64, Float128,
    String,

  GMutability* {.pure.} = enum
    Immutable, Mutable,

  GNodeKind* {.pure.} = enum
    Type, Symbol, Literal,
    Definition, Assignment,
    List,

  # NodeKind* {.pure.} = enum
  #   List,
  #   Definition, Assignment,
  #   Conditional,
  #   Loop, Break, Continue,
  #   Call, Return,
  #   Dot, Bracket,
  #   Prefix, Infix,
  #   Parenthesis,
  #   Symbol, Literal,


type
  GEnumField* = object
    name*: string

  GType* = object
    case kind*: GTypeKind
    of GTypeKind.EnumField: enumField*: GEnumField
    else: discard

  GSymbol* = object
    identifier*: string
    mutability*: GMutability
    typ*: GType
    value*: Option[GNode]

  GLiteral* = object
    case kind*: GLiteralKind
    of GLiteralKind.Nil: discard
    of GLiteralKind.Char..GLiteralKind.UInt64: intValue*: BiggestInt
    of GLiteralKind.Float..GLiteralKind.Float128: floatValue*: float
    of GLiteralKind.String: stringValue*: string

  GDefinition* = object
    symbol*: GSymbol
    value*: Option[GNode]

  GAssignment* = object
    symbol*: GSymbol
    value*: GNode

  GList* = object
    nodes*: seq[GNode]

  GNode* = ref object
    case kind*: GNodeKind
    of GNodeKind.Type: typ*: GType
    of GNodeKind.Symbol: symbol*: GSymbol
    of GNodeKind.Literal: literal*: GLiteral
    of GNodeKind.Definition: definition*: GDefinition
    of GNodeKind.Assignment: assignment*: GAssignment
    of GNodeKind.List: list*: GList

proc newGNode*(s: GType): GNode =
  GNode(kind: GNodeKind.Type, typ: s)

proc newGNode*(s: GSymbol): GNode =
  GNode(kind: GNodeKind.Symbol, symbol: s)

proc newGNode*(s: GLiteral): GNode =
  GNode(kind: GNodeKind.Literal, literal: s)

proc newGNode*(s: GDefinition): GNode =
  GNode(kind: GNodeKind.Definition, definition: s)

proc newGNode*(s: GAssignment): GNode =
  GNode(kind: GNodeKind.Assignment, assignment: s)

proc newGNode*(s: GList): GNode =
  GNode(kind: GNodeKind.List, list: s)