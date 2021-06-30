# type
#   Point2d = object
#     x, y: float

# proc xPlusY(s: Point2d): float =
#   s.x + s.y

# let a = Point2d(x: 3.0, y: 2.0)

# discard a.xPlusY

proc test(a: int): int =
  proc test(a: bool): bool =
    a and true

  a + 1

proc test(a: float): float =
  a + 1.0