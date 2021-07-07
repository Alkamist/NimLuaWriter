import std/macros, general_ast

proc toGSymbolKind(kind: NimSymKind): GSymbolKind =
  case kind:
  of nskType: GSymbolKind.Type
  of nskLet: GSymbolKind.Let
  of nskVar: GSymbolKind.Var
  else: raise newException(IOError, "Unsupported NimSymKind for toGSymbolKind: " & $kind)

proc toGNode*(n: NimNode): GNode

proc symToGNode(n: NimNode): GNode =
  result = newGSymbol()
  result.symbolName = $n
  result.symbolKind = n.symKind.toGSymbolKind
  # result.symbolType = n.getType.toGNode
  # result.symbolOwner = n.owner.toGNode

proc stmtListToGNode(n: NimNode): GNode =
  result = newGStatementList()
  for node in n:
    result.children.add node.toGNode

proc letSectionToGNode(n: NimNode): GNode =
  result = newGStatementList()
  for identDefs in n:
    let
      value = identDefs[^1]
      typeAnnotation = identDefs[^2]
    for varSym in identDefs[0 ..^ 3]:
      var definition = newGLetDefinition()
      definition.letDefinitionSymbol = varSym.toGNode
      definition.letDefinitionType = typeAnnotation.toGNode
      definition.letDefinitionValue = value.toGNode
      result.children.add definition

proc varSectionToGNode(n: NimNode): GNode =
  result = newGStatementList()
  for identDefs in n:
    let
      value = identDefs[^1]
      typeAnnotation = identDefs[^2]
    for varSym in identDefs[0 ..^ 3]:
      var definition = newGVarDefinition()
      definition.varDefinitionSymbol = varSym.toGNode
      definition.varDefinitionType = typeAnnotation.toGNode
      definition.varDefinitionValue = value.toGNode
      result.children.add definition

proc toGNode*(n: NimNode): GNode =
  case n.kind:
  of nnkEmpty: newGEmpty()
  of nnkCharLit: n.intVal.newGCharLiteral
  of nnkIntLit: n.intVal.newGIntLiteral
  of nnkInt8Lit: n.intVal.newGInt8Literal
  of nnkInt16Lit: n.intVal.newGInt16Literal
  of nnkInt32Lit: n.intVal.newGInt32Literal
  of nnkInt64Lit: n.intVal.newGInt64Literal
  of nnkUIntLit: n.intVal.newGUIntLiteral
  of nnkUInt8Lit: n.intVal.newGUInt8Literal
  of nnkUInt16Lit: n.intVal.newGUInt16Literal
  of nnkUInt32Lit: n.intVal.newGUInt32Literal
  of nnkUInt64Lit: n.intVal.newGUInt64Literal
  of nnkFloatLit: n.floatVal.newGFloatLiteral
  of nnkFloat32Lit: n.floatVal.newGFloat32Literal
  of nnkFloat64Lit: n.floatVal.newGFloat64Literal
  of nnkFloat128Lit: n.floatVal.newGFloat128Literal
  of nnkStrLit: n.strVal.newGStringLiteral
  of nnkTripleStrLit: n.strVal.newGStringLiteral
  of nnkIdent: n.strVal.newGIdentifier
  of nnkSym: n.symToGNode
  of nnkStmtList: n.stmtListToGNode
  of nnkLetSection: n.letSectionToGNode
  of nnkVarSection: n.varSectionToGNode
  else: raise newException(IOError, "Unsupported NimNodeKind for toGNode: " & $n.kind)