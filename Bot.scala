/**
 MIT Licensed.

 Copyright (C) 2010 by Greg Funchess

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 */
import java.awt.{Robot, MouseInfo}
import java.awt.event._
import java.math.BigInteger
import scala.collection.mutable._
import io._

object Bot {
  var words: HashMap[BigInteger, List[String]] = new HashMap[BigInteger, List[String]]
  val primes = Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101)  
  val robo = new Robot()

  def wordScore(word: String) = {
    var res = new BigInteger("1")
    for (c <- word.toLowerCase()) {
      var temp = new BigInteger(""+primes(c-'a'))
      res = res.multiply(temp)
    }
    res
  }
  def processWords() = {
    val lines = Source.fromFile("web2.txt").getLines()
    var res = new BigInteger("1")
    for (word <- lines) {
      try {
        var temp: List[String] = words.get(wordScore(word)).get
        temp ++= List(word)
        words += wordScore(word) -> temp
      } catch {
        case e:Exception =>
          words += wordScore(word) -> List(word)
      }
    }
  }

  def anagrams(word: String) = {
    val ws = wordScore(word)
    var res: List[String] = List()
    println(ws)
    for (score <- words.keysIterator) {
      if (score.compareTo(score.gcd(ws)) == 0) {
        res ++= words.get(score).get
      }
    }
    res
  }

  def typeWord(word: String) = {
    // click the area
    robo.mousePress(InputEvent.BUTTON1_MASK)
    robo.mouseRelease(InputEvent.BUTTON1_MASK)
    // type the letters
    for (letter <- word.toUpperCase()) {
      robo.keyPress(letter.toInt)
      robo.keyRelease(letter.toInt)
    }
    // press space
    robo.keyPress(KeyEvent.VK_ENTER)
    robo.keyRelease(KeyEvent.VK_ENTER)
  }

  def main(args: Array[String]) = {
    println("Beginning to process words")
    processWords()
    println("Processed "+words.size+" words")
    var running = true
    while(running) {
      var input = readLine("Input the shuffled word: ")
      if (input == "quit") {
        running = false

      } else {
        println("Delaying for 5 seconds to get ready")
        robo.delay(5000)
        var as = anagrams(input)
        println("Anagrams found are: "+as)
        for (word <- as) {
          typeWord(word)
          robo.delay(100)
        }
      }
    }
    println("done")
  }
}

