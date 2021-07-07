import nim_to_general, general_to_nim

macro test*(nimCode: typed): untyped =
  echo nimCode.toGNode.toNimCode

test:
  type Animal {.pure.} = enum
    Dog, Cat,

  let a = Animal.Dog

  # let
  #   a, b = 1
  #   c = 2