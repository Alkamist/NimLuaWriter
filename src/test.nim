# let a = 1 + (
#     let b = 2
#     b
#   )

# let
#   a = 1
#   b = a.float + 1

# discard b.bool

let b = 5
let a = (if true: b else: 2) + (if true: 1 else: b)
