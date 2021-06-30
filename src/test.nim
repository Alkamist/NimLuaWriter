proc test(a, b = true, c = 3.0): float =
  c

discard test(a = false, c = test(b = false))