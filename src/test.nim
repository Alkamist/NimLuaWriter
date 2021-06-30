# block:
#   let a = 1

# block:
#   let a = 1

proc test(a: int): float =
  10.0

proc test(a: float): int =
  10

discard test(1)
discard test(1.0)
discard test(test(1.0))