# proc test(a, b = true, c = 3.0): float =
#   c

# discard test(a = false, c = test(b = false))




# let a = 0

# proc test(a, b = true): float =
#   a.float

# discard a.float




# let b = if true: 1.0 else: 2.0

# discard b.bool





# let a = 1

# if a == 1: discard 1
# elif a == 2: discard 2
# else: discard 3

# case a:
# of 1: discard 1
# of 2: discard 2
# else: discard 3