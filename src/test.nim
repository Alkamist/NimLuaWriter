# type
#   Point2d = object
#     x, y: float

# proc `+`(a, b: Point2d): Point2d =
#   result.x = a.x + b.x
#   result.y = a.y + b.y

# let
#   a = Point2d(x: 1.0, y: 2.0)
#   b = Point2d(x: 3.0, y: 4.0)

# echo a + b

#proc echo(message: string) {.importcpp: "reaper.ShowConsoleMsg(# .. \"\n\")".}
# proc test(arg1, arg2, arg3, arg4: int) {.importcpp: "importTest(@)".}

#echo $5