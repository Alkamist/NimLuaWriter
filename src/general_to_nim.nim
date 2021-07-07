import general_ast

proc toNimCode*(n: GNode): string

proc typeToNimCode(n: GNode): string =
  case n.typeKind:
  of GTypeKind.Nil: "nil"
  of GTypeKind.Char: "char"
  of GTypeKind.Int: "int"
  of GTypeKind.Int8: "int8"
  of GTypeKind.Int16: "int16"
  of GTypeKind.Int32: "int32"
  of GTypeKind.Int64: "int64"
  of GTypeKind.UInt: "uint"
  of GTypeKind.UInt8: "uint8"
  of GTypeKind.UInt16: "uint16"
  of GTypeKind.UInt32: "uint32"
  of GTypeKind.UInt64: "uint64"
  of GTypeKind.Float: "float"
  of GTypeKind.Float32: "float32"
  of GTypeKind.Float64: "float64"
  of GTypeKind.Float128: "float128"
  of GTypeKind.String: "string"
  of GTypeKind.Bool: "bool"

proc symbolToNimCode(n: GNode): string =
  n.symbolName

proc letDefinitionToNimCode(n: GNode): string =
  result.add "let "
  result.add n.letDefinitionSymbol.toNimCode
  if n.letDefinitionType.kind != GNodeKind.Empty:
    result.add ": "
    result.add n.letDefinitionType.toNimCode
  if n.letDefinitionValue.kind != GNodeKind.Empty:
    result.add " = "
    result.add n.letDefinitionValue.toNimCode

proc varDefinitionToNimCode(n: GNode): string =
  result.add "var "
  result.add n.varDefinitionSymbol.toNimCode
  if n.varDefinitionType.kind != GNodeKind.Empty:
    result.add ": "
    result.add n.varDefinitionType.toNimCode
  if n.varDefinitionValue.kind != GNodeKind.Empty:
    result.add " = "
    result.add n.varDefinitionValue.toNimCode

proc statementListToNimCode(n: GNode): string =
  let lastId = n.children.len - 1
  for i, node in n.children:
    result.add node.toNimCode
    if i < lastId:
      result.add "\n"

proc toNimCode*(n: GNode): string =
  case n.kind:
  of GNodeKind.Empty: ""
  of GNodeKind.NilLiteral: "nil"
  of GNodeKind.CharLiteral..GNodeKind.UInt64Literal: $n.intValue
  of GNodeKind.FloatLiteral..GNodeKind.Float128Literal: $n.floatValue
  of GNodeKind.Identifier, GNodeKind.StringLiteral: "\"" & n.stringValue & "\""
  of GNodeKind.BoolLiteral: $n.boolValue
  of GNodeKind.Type: n.typeToNimCode
  of GNodeKind.Symbol: n.symbolToNimCode
  of GNodeKind.LetDefinition: n.letDefinitionToNimCode
  of GNodeKind.VarDefinition: n.varDefinitionToNimCode
  of GNodeKind.StatementList: n.statementListToNimCode