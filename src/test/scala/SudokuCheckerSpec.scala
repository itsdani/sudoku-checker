import org.scalatest.{Matchers, WordSpec}

class SudokuCheckerSpec extends WordSpec with Matchers {

  "SudokuChecker" when {
    "input is not square" should {
      "return false" in {
        val checker = SudokuChecker
        val board = List(List(1, 2, 3), List(4, 5, 6))
        val result = checker.check(board)

        result shouldBe false
      }
    }
    "solution is correct" should {
      "return true" in {
        val checker = SudokuChecker
        val board = List(
          List(1, 2, 3, 4, 5, 6, 7, 8, 9),
          List(4, 5, 6, 7, 8, 9, 1, 2, 3),
          List(7, 8, 9, 1, 2, 3, 4, 5, 6),
          List(2, 3, 1, 5, 6, 4, 8, 9, 7),
          List(5, 6, 4, 8, 9, 7, 2, 3, 1),
          List(8, 9, 7, 2, 3, 1, 5, 6, 4),
          List(3, 1, 2, 6, 4, 5, 9, 7, 8),
          List(6, 4, 5, 9, 7, 8, 3, 1, 2),
          List(9, 7, 8, 3, 1, 2, 6, 4, 5),
        )
        val result = checker.check(board)

        result shouldBe true
      }
    }

    "a solution element is higher than allowed" should {
      "return false" in {
        val board = List(
          List(1, 2, 3),
          List(4, 5, 6),
          List(7, 8, 9)
        )
        val result = SudokuChecker.check(board)

        result shouldBe false
      }
    }

    "a solution element is lower than 1" should {
      "return false" in {
        val board = List(
          List(0, 2, 3),
          List(2, 3, 1),
          List(3, 1, 2)
        )
        val result = SudokuChecker.check(board)

        result shouldBe false
      }
    }

    "an element is duplicated in a row" should {
      "return false" in {
        val board = List(
          List(1, 2, 2),
          List(2, 3, 1),
          List(3, 1, 3)
        )

        val result = SudokuChecker.check(board)

        result shouldBe false
      }
    }

    "an element is duplicated in a column" should {
      "return false" in {
        val board = List(
          List(1, 2, 3),
          List(1, 3, 2),
          List(2, 3, 1)
        )

        val result = SudokuChecker.check(board)

        result shouldBe false
      }
    }

    "an element is duplicated in a subsquare" should {
      "return false" in {
        val board = List(
          List(1, 2, 3,   4, 5, 6,   7, 8, 9),
          List(2, 3, 4,   5, 6, 7,   8, 9, 1),
          List(3, 4, 5,   6, 7, 8,   9, 1, 2),
          List(4, 5, 6,   7, 8, 9,   1, 2, 3),
          List(5, 6, 7,   8, 9, 1,   2, 3, 4),
          List(6, 7, 8,   9, 1, 2,   3, 4, 5),
          List(7, 8, 9,   1, 2, 3,   4, 5, 6),
          List(8, 9, 1,   2, 3, 4,   5, 6, 7),
          List(9, 1, 2,   3, 4, 5,   6, 7, 9),
        )

        val result = SudokuChecker.check(board)

        result shouldBe false
      }
    }

    "called for a correct subsquare" should {
      "return true" in {
        val subSquare = List(
          List(1, 2, 3),
          List(4, 5, 6),
          List(7, 8, 9)
        )

        val result = SudokuChecker.checkSubSquare(subSquare)
        result shouldBe true
      }
    }

    "split into subsquares" should {
      "return the first subsquare in a list" in {

        val board = List(
          List(1, 2, 3,   4, 5, 6,   7, 8, 9),
          List(2, 3, 4,   5, 6, 7,   8, 9, 1),
          List(3, 4, 5,   6, 7, 8,   9, 1, 2),
          List(4, 5, 6,   7, 8, 9,   1, 2, 3),
          List(5, 6, 7,   8, 9, 1,   2, 3, 4),
          List(6, 7, 8,   9, 1, 2,   3, 4, 5),
          List(7, 8, 9,   1, 2, 3,   4, 5, 6),
          List(8, 9, 1,   2, 3, 4,   5, 6, 7),
          List(9, 1, 2,   3, 4, 5,   6, 7, 9),
        )

        val expectedSubsquare = List(
          List(1, 2, 3),
          List(2, 3, 4),
          List(3, 4, 5)
        )

        val result = SudokuChecker.splitIntoSubsquares(board).head;
        result shouldBe expectedSubsquare
      }
    }

    "split into subsquares" should {
      "return the correct number of subsquares" in {
        val board = List(
          List(1, 2, 3,   4, 5, 6,   7, 8, 9),
          List(2, 3, 4,   5, 6, 7,   8, 9, 1),
          List(3, 4, 5,   6, 7, 8,   9, 1, 2),
          List(4, 5, 6,   7, 8, 9,   1, 2, 3),
          List(5, 6, 7,   8, 9, 1,   2, 3, 4),
          List(6, 7, 8,   9, 1, 2,   3, 4, 5),
          List(7, 8, 9,   1, 2, 3,   4, 5, 6),
          List(8, 9, 1,   2, 3, 4,   5, 6, 7),
          List(9, 1, 2,   3, 4, 5,   6, 7, 9),
        )

        val result = SudokuChecker.splitIntoSubsquares(board)
        result.length shouldBe board.length
      }
    }
  }
}
