import std/macros, makeluafriendly

expandMacros:
  makeLuaFriendly:
    let a = 1

    discard a.bool