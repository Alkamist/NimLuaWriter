import std/[options, macros], general_ast

proc toGNode*(n: NimNode): GNode

proc symToGNode(n: NimNode): GNode =
  echo n.getType.typeKind
  GSymbol().newGNode
  # symbol.identifier = $n
  # symbol.mutability = GMutability.Immutable
  # symbol.typ = GType(kind: GTypeKind.Int)
  # symbol.value = some(
  #   GLiteral(
  #     kind: GLiteralKind.Int,
  #     intValue: value.intVal,
  #   ).newGNode
  # )

proc stmtListToGNode(n: NimNode): GNode =
  var list = GList()

  for node in n:
    list.nodes.add node.toGNode

  list.newGNode

proc letSectionToGNode(n: NimNode): GNode =
  var list = GList()

  for identDefs in n:
    let value = identDefs[^1]
    for varSym in identDefs[0 ..^ 3]:
      var definition = GDefinition(symbol: varSym.toGNode.symbol)
      list.nodes.add definition.newGNode

  list.newGNode

proc toGNode*(n: NimNode): GNode =
  case n.kind:
  of nnkSym: n.symToGNode
  of nnkStmtList: n.stmtListToGNode
  of nnkLetSection: n.letSectionToGNode
  else: raise newException(IOError, "Unsupported NimNode kind: " & $n.kind)