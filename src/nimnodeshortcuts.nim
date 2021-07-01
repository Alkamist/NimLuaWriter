import std/[macros, options]

template lastNode*(n: NimNode): untyped =
  n[n.len-1]

template identDefsVars*(n: NimNode): untyped =
  n[0..<n.len-2]

template identDefsType*(n: NimNode): untyped =
  n[n.len-2]

template identDefsValue*(n: NimNode): untyped =
  n[n.len-1]

template objConstrValues*(n: NimNode): untyped =
  n[1..<n.len]

template formalParamsIdentDefs*(n: NimNode): untyped =
  n[1..<n.len]

template formalParamsReturnType*(n: NimNode): untyped =
  n[0]

template procDefName*(n: NimNode): untyped =
  n[0]

template procDefFormalParams*(n: NimNode): untyped =
  n[3]

template procDefBody*(n: NimNode): untyped =
  n[6]

proc procDefResultName*(n: NimNode): Option[NimNode] =
  if n.procDefFormalParams[0].kind != nnkEmpty:
    return some(n[7])

template callName*(n: NimNode): untyped =
  n[0]

template callValues*(n: NimNode): untyped =
  n[1..<n.len]

template caseStmtSelector*(n: NimNode): untyped =
  n[0]

template caseStmtBranches*(n: NimNode): untyped =
  n[1..<n.len]

template infixOp*(n: NimNode): untyped =
  n[0]

template infixLeft*(n: NimNode): untyped =
  n[1]

template infixRight*(n: NimNode): untyped =
  n[2]