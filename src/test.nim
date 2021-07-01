# proc test(a, b = true, c = 3.0): float =
#   c

# discard test(a = false, c = test(b = false))




# let a = 0

# proc test(a, b = true): float =
#   a.float
let
  a = 1.0
  b = block:
    a

discard b.bool



# discard a.float



# let a = 1

# if a == 1: discard 1
# elif a == 2: discard 2
# else: discard 3

# case a:
# of 1: discard 1
# of 2: discard 2
# else: discard 3



# let
#   a = true
#   b = if a: 1.0 else: 2.0

# let
#   c = 1
#   d = case c:
#     of 1: 1.0
#     of 2: 2.0
#     else: 3.0

# let
#   e = true
#   f = block:
#     e

# let
#   g = (
#     let h = true
#     h
#   )

#discard b.bool