type
  GTypeKind* {.pure.} = enum
    Nil,
    Char,
    Int, Int8, Int16, Int32, Int64,
    UInt, UInt8, UInt16, UInt32, UInt64,
    Float, Float32, Float64, Float128,
    String, Bool,

  GSymbolKind* {.pure.} = enum
    Type, Let, Var,

  GNodeKind* {.pure.} = enum
    Empty,
    Identifier,
    CharLiteral,
    IntLiteral, Int8Literal, Int16Literal, Int32Literal, Int64Literal,
    UIntLiteral, UInt8Literal, UInt16Literal, UInt32Literal, UInt64Literal,
    FloatLiteral, Float32Literal, Float64Literal, Float128Literal,
    StringLiteral, BoolLiteral,
    NilLiteral,
    Type, Symbol,
    StatementList,
    LetDefinition, VarDefinition,

  GNode* = ref object
    case kind*: GNodeKind
    of GNodeKind.Empty, GNodeKind.NilLiteral: discard
    of GNodeKind.CharLiteral..GNodeKind.UInt64Literal: intValue*: BiggestInt
    of GNodeKind.FloatLiteral..GNodeKind.Float128Literal: floatValue*: BiggestFloat
    of GNodeKind.Identifier, GNodeKind.StringLiteral: stringValue*: string
    of GNodeKind.BoolLiteral: boolValue*: bool
    of GNodeKind.Type: typeKind*: GTypeKind
    of GNodeKind.Symbol:
      symbolName*: string
      symbolKind*: GSymbolKind
      symbolType*: GNode
      symbolOwner*: GNode
    of GNodeKind.LetDefinition:
      letDefinitionSymbol*: GNode
      letDefinitionType*: GNode
      letDefinitionValue*: GNode
    of GNodeKind.VarDefinition:
      varDefinitionSymbol*: GNode
      varDefinitionType*: GNode
      varDefinitionValue*: GNode
    else: children*: seq[GNode]

proc newGEmpty*(): GNode =
  GNode(kind: GNodeKind.Empty)

proc newGIdentifier*(value: string): GNode =
  GNode(kind: GNodeKind.Identifier, stringValue: value)

proc newGCharLiteral*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.CharLiteral, intValue: value)

proc newGIntLiteral*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.IntLiteral, intValue: value)

proc newGInt8Literal*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.Int8Literal, intValue: value)

proc newGInt16Literal*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.Int16Literal, intValue: value)

proc newGInt32Literal*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.Int32Literal, intValue: value)

proc newGInt64Literal*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.Int64Literal, intValue: value)

proc newGUIntLiteral*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.UIntLiteral, intValue: value)

proc newGUInt8Literal*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.UInt8Literal, intValue: value)

proc newGUInt16Literal*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.UInt16Literal, intValue: value)

proc newGUInt32Literal*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.UInt32Literal, intValue: value)

proc newGUInt64Literal*(value: BiggestInt): GNode =
  GNode(kind: GNodeKind.UInt64Literal, intValue: value)

proc newGFloatLiteral*(value: BiggestFloat): GNode =
  GNode(kind: GNodeKind.FloatLiteral, floatValue: value)

proc newGFloat32Literal*(value: BiggestFloat): GNode =
  GNode(kind: GNodeKind.Float32Literal, floatValue: value)

proc newGFloat64Literal*(value: BiggestFloat): GNode =
  GNode(kind: GNodeKind.Float64Literal, floatValue: value)

proc newGFloat128Literal*(value: BiggestFloat): GNode =
  GNode(kind: GNodeKind.Float128Literal, floatValue: value)

proc newGStringLiteral*(value: string): GNode =
  GNode(kind: GNodeKind.StringLiteral, stringValue: value)

proc newGBoolLiteral*(value: bool): GNode =
  GNode(kind: GNodeKind.BoolLiteral, boolValue: value)

proc newGNilLiteral*(): GNode =
  GNode(kind: GNodeKind.NilLiteral)

proc newGType*(): GNode =
  GNode(kind: GNodeKind.Type)

proc newGSymbol*(): GNode =
  GNode(kind: GNodeKind.Symbol)

proc newGLetDefinition*(): GNode =
  GNode(kind: GNodeKind.LetDefinition)

proc newGVarDefinition*(): GNode =
  GNode(kind: GNodeKind.VarDefinition)

proc newGStatementList*(): GNode =
  GNode(kind: GNodeKind.StatementList)