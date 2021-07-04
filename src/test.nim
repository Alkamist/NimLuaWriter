type
  Point2d = object
    x, y: float

proc translate(s, by: Point2d): Point2d =
  result.x = s.x + by.x
  result.y = s.y + by.y

proc translate(s: var Point2d, by: Point2d): Point2d =
  s.x = s.x + by.x
  s.y = s.y + by.y

let pointLet = Point2d(y: 5.0)
var pointVar = Point2d(y: 5.0)

discard pointLet.translate(Point2d(x: 1.0, y: 2.0))
discard pointVar.translate(Point2d(x: 1.0, y: 2.0))


# proc test(a = 1): int =
#   a + 2

# discard test(if true: 5 else: 1)


# let a = if true: 5 else: 1