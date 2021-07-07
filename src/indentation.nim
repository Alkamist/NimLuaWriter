const
  IndentLevelUp* = "@INDENT_UP"
  IndentLevelDown* = "@INDENT_DOWN"

proc addIndentation*(text: string, spaces: int): string =
  result = ""

  var
    indentationStr = ""
    indentationLevel = 0

  for _ in 0 ..< spaces:
    indentationStr.add(" ")

  template indent(): untyped =
    for _ in 0 ..< indentationLevel:
      result.add indentationStr

  var i = 0
  while i < text.len:
    let charsTilEof = text.len - i

    if charsTilEof >= IndentLevelUp.len and
       text[i ..< i + IndentLevelUp.len] == IndentLevelUp:
      indentationLevel += 1
      i += IndentLevelUp.len
      indent()

    elif charsTilEof >= IndentLevelDown.len and
         text[i ..< i + IndentLevelDown.len] == IndentLevelDown:
      indentationLevel -= 1
      i += IndentLevelDown.len
      indent()

    else:
      if i > 0 and text[i - 1] == '\n':
        indent()

      result.add(text[i])
      i += 1