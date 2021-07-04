# let a = case 5:
#   of 1: 1
#   of 2: 2
#   else: 3

# if true:
#   a = 1
# else:
#   a = 2


# var a = 1

# case 5:
# of 1: a = 1
# of 2: a = 2
# else: a = 3


# let b = 1
# let a = (if b == 2: 2 else: 3) + (if b == 3: 3 else: 2)

# discard a.bool


# type
#   Point2d = object
#     x, y: float

#   TestEnum = enum
#     teField0,
#     teField1,

# let a = teField0

# let b = if a == teField1: 1 else: 2

# let point = Point2d(y: 3.0)


proc test(a: int): int =
  var b = 2
  var c = 3
  if b == 1:
    a
  else:
    c