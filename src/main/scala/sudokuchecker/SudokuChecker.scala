object SudokuChecker {
  def check(board: List[List[Int]]): Boolean = {
    isBoardSquare(board) &&
      isInRange(board) &&
      areRowElementsDistinct(board) &&
      areRowElementsDistinct(board.transpose) &&
      areSubsquareElementsDistinct(board)
  }

  private def isBoardSquare(board: List[List[Int]]): Boolean = {
    val dimension = board.length
    board.forall(row => row.length == dimension)
  }

  private def isInRange(board: List[List[Int]]) = {
    val dimension = board.length
    board.forall(row => row.forall(element => element <= dimension && element > 0))
  }

  private def areRowElementsDistinct(board: List[List[Int]]): Boolean = {
    board.forall(row => row.distinct.length == row.length)
  }

  private def areSubsquareElementsDistinct(board: List[List[Int]]): Boolean = {
    val subsquares = splitIntoSubsquares(board)
    print(subsquares)
    subsquares.forall(subsquare => checkSubSquare(subsquare))
  }

  def checkSubSquare(board: List[List[Int]]): Boolean = {
    board.flatten.distinct.length == board.flatten.length
  }

  def splitIntoSubsquares(board: List[List[Int]]): List[List[List[Int]]] = {
    val subsquareSize = math.round(math.sqrt(board.length)).toInt
    val startpoints = (0 to (subsquareSize - 1)).toList

    val product = for {x <- startpoints; y <- startpoints} yield (x * subsquareSize, y * subsquareSize)

    product.map(params => params match {
      case (colIndex, rowIndex) => getSubSquare(board, colIndex, rowIndex, subsquareSize)
    })
  }

  private def getSubSquare(board: List[List[Int]], colIndex: Int, rowIndex: Int, subsquareDimension: Int): List[List[Int]] = {
    board.drop(colIndex).take(subsquareDimension).map(row => row.drop(rowIndex).take(subsquareDimension))
  }
}
