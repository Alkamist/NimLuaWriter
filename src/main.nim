import general_ast, nim_to_general, general_to_nim, indentation

macro test*(nimCode: typed): untyped =
  echo nimCode.toGNode.removeAssignmentExpressions.cleanGNode.toNimCode.addIndentation(2)

test:
  let a = (
    let b = 2
    b
  ) + (
    let c = 3
    c
  )