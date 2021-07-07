import std/options, general_ast

proc toNimCode*(n: GNode): string

proc toNimCode(n: GType): string =
  case n.kind:
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
  of GTypeKind.Object: "object"
  of GTypeKind.Enum: "enum"
  of GTypeKind.EnumField: n.enumField.name

proc toNimCode(n: GLiteral): string =
  case n.kind:
  of GLiteralKind.Nil: "nil"
  of GLiteralKind.Char..GLiteralKind.UInt64: $n.intValue
  of GLiteralKind.Float..GLiteralKind.Float128: $n.floatValue
  of GLiteralKind.String: "\"" & n.stringValue & "\""

proc toNimCode(n: GSymbol): string =
  n.identifier

proc toNimCode(n: GDefinition): string =
  case n.symbol.mutability:
  of GMutability.Immutable: result.add "let "
  of GMutability.Mutable: result.add "var "

  result.add n.symbol.identifier
  result.add ": "
  result.add n.symbol.typ.toNimCode

  if n.value.isSome:
    result.add " = "
    result.add n.value.get.toNimCode

# proc toNimCode(n: GAssignment): string =
#   result.add n.symbol.toNimCode
#   result.add " = "
#   result.add n.symbol.value.toNimCode

proc toNimCode(n: GList): string =
  let lastId = n.nodes.len - 1
  for i, node in n.nodes:
    result.add node.toNimCode
    if i < lastId:
      result.add "\n"

proc toNimCode*(n: GNode): string =
  case n.kind:
  of GNodeKind.Type: n.typ.toNimCode
  of GNodeKind.Symbol: n.symbol.toNimCode
  of GNodeKind.Definition: n.definition.toNimCode
  #of NodeKind.Assignment: n.assignment.toNimCode
  of GNodeKind.List: n.list.toNimCode
  else: ""