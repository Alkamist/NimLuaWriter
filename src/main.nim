import nimtolua

let luaCode = writeLua(2):
  include test

echo luaCode

# import std/macros

# dumptree:
#   let
#     a, b = 1
#     c = 2