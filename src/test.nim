proc test(a, b = true, c = 3.0): float =
  c

discard test(a = false, c = test(b = false))


# let a = 0

# proc test(a = true): float =
#   a.float

# discard a.float


# let b = 0

# discard b.float