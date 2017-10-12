package keypad

class Keypad(words: List[String]) {
  def suggest(pressedKeys: String): String = pressedKeys match {
    case "" => ""
    case _  => matchesFor(pressedKeys).mkString(" ")
  }

  private def matchesFor(pressedKeys: String) = {
    val pressedText = rawText(pressedKeys)
    val matchingWords = words.filter(_.startsWith(pressedText))
    pressedText :: matchingWords
  }

  private def rawText(pressedKeys: String) = {
    pressedKeys.split(" ").map(mapKeyGroupToLetter).mkString("")
  }

  def mapKeyGroupToLetter(keyGroup: String): Char = {
    val pressedKey = keyGroup(0)
    val lettersOnKey = keyMap(pressedKey)
    val indexOfLetter = (keyGroup.length - 1) % lettersOnKey.length
    lettersOnKey(indexOfLetter)
  }

  val keyMap: Map[Char, String] = {
    Map(
      '2' -> "abc",
      '3' -> "def",
      '4' -> "ghi",
      '5' -> "jkl",
      '6' -> "mno",
      '7' -> "pqrs",
      '8' -> "tuv",
      '9' -> "wxyz"
    )
  }
}
