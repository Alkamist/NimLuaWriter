var
  a = 5
  b = case a:
    of 1:
      var c = if true: 1.0 else: 2.0
      2.0
    else:
      3.0

discard b.bool


# var b = case 5:
#   of 1:
#     var c = if true: 1.0 else: 2.0
#     2.0
#   else:
#     3.0