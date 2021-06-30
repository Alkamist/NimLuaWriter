proc test(a: float): float =
  a

proc test(a: bool): float =
  1.0

discard test(test(true))
#discard test(true)