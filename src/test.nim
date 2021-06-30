proc onePointZero(): int =
  1

proc test(a = onePointZero()): float =
  2.0

#discard test(test(1))
# discard test(1.0)







# proc test(a = true): float =
#   1.0

#discard test(test(true))
#discard test(true)