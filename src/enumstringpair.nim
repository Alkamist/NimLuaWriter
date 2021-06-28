import std/macros

macro enumStringPair*(enumName, members: untyped): untyped =
  result = nnkStmtList.newTree()

  # Enum definition:
  var enumDef = nnkTypeSection.newTree(
    nnkTypeDef.newTree(
      nnkPostfix.newTree(
        ident("*"),
        enumName,
      ),
      newEmptyNode(),
      nnkEnumTy.newTree(
        newEmptyNode(),
      ),
    ),
  )

  for member in members:
    enumDef[0][2].add(member[0])

  # To string converter:
  var toStringProc = nnkProcDef.newTree(
    nnkPostfix.newTree(
      ident("*"),
      ident("toString"),
    ),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      ident("string"),
      nnkIdentDefs.newTree(
        ident("kind"),
        enumName,
        newEmptyNode(),
      ),
    ),
    newEmptyNode(),
    newEmptyNode(),
    nnkCaseStmt.newTree(
      ident("kind"),
    ),
  )

  for member in members:
    toStringProc[6].add(nnkOfBranch.newTree(
      member[0],
      member[1],
    ))

  # To enum converter:
  var toEnumProc = nnkProcDef.newTree(
    nnkPostfix.newTree(
      ident("*"),
      ident("to" & enumName.strVal),
    ),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      enumName,
      nnkIdentDefs.newTree(
        ident("text"),
        ident("string"),
        newEmptyNode(),
      ),
    ),
    newEmptyNode(),
    newEmptyNode(),
    nnkCaseStmt.newTree(
      ident("text"),
    ),
  )

  for member in members:
    toEnumProc[6].add(nnkOfBranch.newTree(
      member[1],
      member[0],
    ))

  let enumStr = enumName.strVal
  let errorAst = quote do:
    raise newException(IOError, "Cannot convert string to enum: " & `enumStr`)

  toEnumProc[6].add(nnkElse.newTree(errorAst))

  result.add(enumDef)
  result.add(toStringProc)
  result.add(toEnumProc)

enumStringPair(LuaOperatorKind):
  (lokPlus, "+")
  (lokMinus, "-")
  (lokStar, "*")
  (lokSlash, "/")
  (lokMod, "%")
  (lokCaret, "^")
  (lokEquals, "=")
  (lokNotEquals, "~=")
  (lokEqualsEquals, "==")
  (lokGreater, ">")
  (lokGreaterEquals, ">=")
  (lokLesser, "<")
  (lokLesserEquals, "<=")
  (lokAnd, "and")
  (lokOr, "or")
  (lokNot, "not")
  (lokLength, "#")
  (lokConcat, "..")

echo "..".toLuaOperatorKind