import std/options

type
  BitPrecisionKind* {.pure.} = enum
    SetBitCount, ArchitectureSpecific,

  IntegerKind* {.pure.} = enum
    Signed, Unsigned,

  IntegerRepresentation* {.pure.} = enum
    Numerical, Character,

  NumberKind* {.pure.} = enum
    Integer, FloatingPoint

  TypeKind* {.pure.} = enum
    Void, Number, Boolean, String,

  Mutability* {.pure.} = enum
    Immutable, Mutable,

  ConditionalKind* {.pure.} = enum
    If, Case,

  LoopKind* {.pure.} = enum
    For, While,

  NodeKind* {.pure.} = enum
    Symbol, Definition, Assignment,

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
  BitPrecision* = object
    case kind*: BitPrecisionKind
    of BitPrecisionKind.SetBitCount: bitCount*: int
    of BitPrecisionKind.ArchitectureSpecific: discard

  Number* = object
    bitPrecision*: BitPrecision
    case kind*: NumberKind
    of NumberKind.Integer:
      integerKind*: IntegerKind
      integerValue*: BiggestInt
      integerRepresentation*: IntegerRepresentation
    of NumberKind.FloatingPoint:
      floatingPointValue*: BiggestFloat

  Boolean* = object
    value*: bool

  String* = object
    value*: string

  Symbol* = object
    mutability*: Mutability
    case kind*: TypeKind
    of TypeKind.Void: discard
    of TypeKind.Number: numberValue*: Number
    of TypeKind.Boolean: booleanValue*: Boolean
    of TypeKind.String: stringValue*: String

  Definition* = object
    symbol*: Symbol
    value*: Node

  Assignment* = object
    symbol*: Symbol
    value*: Node

  Node* = ref object
    case kind*: NodeKind
    of NodeKind.Symbol: symbol*: Symbol
    of NodeKind.Definition: definition*: Definition
    of NodeKind.Assignment: assignment*: Assignment