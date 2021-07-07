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
    CharLiteral,
    IntLiteral, Int8Literal, Int16Literal, Int32Literal, Int64Literal,
    UIntLiteral, UInt8Literal, UInt16Literal, UInt32Literal, UInt64Literal,
    FloatLiteral, Float32Literal, Float64Literal, Float128Literal,
    StringLiteral, BoolLiteral,
    NilLiteral,
    Type, Identifier, Symbol,
    StatementList, StatementListExpression,
    LetDefinition, VarDefinition,
    Infix,

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
      #symbolOwner*: GNode
    of GNodeKind.LetDefinition:
      letDefinitionSymbol*: GNode
      letDefinitionType*: GNode
      letDefinitionValue*: GNode
    of GNodeKind.VarDefinition:
      varDefinitionSymbol*: GNode
      varDefinitionType*: GNode
      varDefinitionValue*: GNode
    of GNodeKind.Infix:
      infixOperator*: GNode
      infixLeft*: GNode
      infixRight*: GNode
    else: children*: seq[GNode]

#########################################################

proc newGEmpty*(): GNode =
  GNode(kind: GNodeKind.Empty)

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

proc newGType*(kind: GTypeKind): GNode =
  GNode(kind: GNodeKind.Type, typeKind: kind)

proc newGIdentifier*(value: string): GNode =
  GNode(kind: GNodeKind.Identifier, stringValue: value)

proc newGSymbol*(name: string, kind: GSymbolKind, typeNode: GNode): GNode =
  GNode(kind: GNodeKind.Symbol,
    symbolName: name,
    symbolKind: kind,
    symbolType: typeNode,
  )

proc newGStatementList*(children = newSeq[GNode]()): GNode =
  GNode(kind: GNodeKind.StatementList,
    children: children,
  )

proc newGStatementListExpression*(children = newSeq[GNode]()): GNode =
  GNode(kind: GNodeKind.StatementListExpression,
    children: children,
  )

proc newGLetDefinition*(symbol: GNode, typeNode, value = newGEmpty()): GNode =
  GNode(kind: GNodeKind.LetDefinition,
    letDefinitionSymbol: symbol,
    letDefinitionType: typeNode,
    letDefinitionValue: value,
  )

proc newGVarDefinition*(symbol: GNode, typeNode, value = newGEmpty()): GNode =
  GNode(kind: GNodeKind.VarDefinition,
    varDefinitionSymbol: symbol,
    varDefinitionType: typeNode,
    varDefinitionValue: value,
  )

proc newGInfix*(operator, left, right: GNode): GNode =
  GNode(kind: GNodeKind.Infix,
    infixOperator: operator,
    infixLeft: left,
    infixRight: right,
  )

#########################################################

const
  AssignmentExpressions = [
    GNodeKind.StatementListExpression, GNodeKind.Infix,
  ]
  ListNodes = [
    GNodeKind.StatementList, GNodeKind.StatementListExpression,
  ]

proc expressionsAndAssignment*(symbol: GNode, value: GNode): (GNode, GNode) =
  if value.kind == GNodeKind.Infix:
    let (leftExpressions, leftAssignment) = expressionsAndAssignment(symbol, value.infixLeft)
    let (rightExpressions, rightAssignment) = expressionsAndAssignment(symbol, value.infixRight)

    let expressions = newGStatementList(@[
      leftExpressions, rightExpressions
    ])

    let assignment = newGInfix(
      operator = value.infixOperator,
      left = leftAssignment,
      right = rightAssignment,
    )

    result = (expressions, assignment)

  else:
    let tempVar = newGSymbol(
      name = "tempVar",
      kind = GSymbolKind.Var,
      typeNode = symbol.symbolType
    )

    let tempDefinition = newGVarDefinition(
      symbol = tempVar,
      typeNode = tempVar.symbolType,
      value = newGEmpty(),
    )

    let expression = newGStatementList(@[
      tempDefinition, value
    ])

    result = (expression, tempVar)

proc removeAssignmentExpressions*(n: GNode): GNode =
  case n.kind:
  of GNodeKind.StatementList:
    result = newGStatementList()
    for child in n.children:
      result.children.add child.removeAssignmentExpressions

  of GNodeKind.LetDefinition:
    if n.letDefinitionValue.kind in AssignmentExpressions:
      let (expressions, assignment) = expressionsAndAssignment(n.letDefinitionSymbol,
                                                               n.letDefinitionValue)

      let definition = newGLetDefinition(
        symbol = n.letDefinitionSymbol,
        typeNode = n.letDefinitionType,
        value = assignment,
      )

      result = newGStatementList(@[
        expressions, definition
      ])
    else:
      result = n

  else:
    result = n

proc isExtraneous*(n: GNode): bool =
  n.kind == GNodeKind.Empty or
  (n.kind in ListNodes and n.children.len < 1)

proc cleanGNode*(n: GNode): GNode =
  case n.kind:
  of ListNodes:
    result = GNode(kind: n.kind)

    for child in n.children:
      if child.isExtraneous:
        continue

      result.children.add child.cleanGNode
  else:
    result = n