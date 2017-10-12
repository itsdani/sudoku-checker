package keypad

import org.scalatest.{Matchers, WordSpec}

class KeypadSpec extends WordSpec with Matchers {
  "Keypad" when {
    "given empty input" should {
      "return empty output" in {
        val words = List("a", "b")
        val keypad = new Keypad(words)
        keypad.suggest("") shouldBe ""
      }
    }
    "dictionary is empty" when {
      val words = List()
      val keypad = new Keypad(words)
      "pressed a key only once" should {
        "return first character on key" in {
          keypad.suggest("2") shouldBe "a"
          keypad.suggest("3") shouldBe "d"
        }
      }
      "pressed a key twice" should {
        "return second character on key" in {
          keypad.suggest("22") shouldBe "b"
        }
      }
      "pressed two different keys" should {
        "return string of two letters" in {
          keypad.suggest("33 6") shouldBe "em"
        }
      }
      "pressed a key more than number of letters on the digit" should {
        "use the letter modulo cucc" in {
          keypad.suggest("2222") shouldBe "a"
        }
      }
    }

    "dictionary contains a word starting with the input" should {
      "return that word" in {
        val words = List("almafa", "banana")
        val keypad = new Keypad(words)
        keypad.suggest("2") shouldBe "a almafa"
        keypad.suggest("2 555 6 2") shouldBe "alma almafa"
      }
    }
  }
}
