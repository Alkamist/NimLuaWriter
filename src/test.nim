type
  Point2d = object
    x, y: float

proc xPlusY(s: Point2d): float =
  s.x + s.y

let a = Point2d(x: 3.0, y: 2.0)

discard a.xPlusY