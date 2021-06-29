type
  Point2d = object
    x, y: float

let
  a = Point2d(x: 1.0, y: 2.0)
  b = if true: 1 else: 2