# proc echo(message: string) {.importcpp: "print(@)".}

# type
#   Point2d = object
#     x, y: float

# proc `+`(a, b: Point2d): Point2d =
#   result.x = a.x + b.x
#   result.y = a.y + b.y

# let
#   a = Point2d(x: 1.0, y: 2.0)
#   b = Point2d(x: 3.0, y: 4.0)

# discard a + b


proc `-*-`(a, b: int): int =
  a - b * b - a

let a, b = 1

discard a -*- b