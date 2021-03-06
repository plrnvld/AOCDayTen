import scala.io.Source
import scala.collection.mutable.Stack

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        
        println(s"Lines count: ${lines.size}")
        val notCorrupted = lines.map(checkSyntax).filter(c => c.calcScore() == 0)

        println(s"Total not corrupted: ${notCorrupted.size}")

        val scores = notCorrupted.map(getStackScore)

        println(scores)

        val middleIndex = (notCorrupted.size - 1)/2
        val middleScore = scores.sorted.toList(middleIndex)

        println(s"Result: ${middleScore}")
    }

    def getStackScore(syntaxPairCount: SyntaxPairCount): Long = {
        var itemsOnStack = syntaxPairCount.stack.toList

        println(s"Stack: ${itemsOnStack}")

        val score = itemsOnStack.foldLeft(0.toLong)((res, i) => res * 5 + i)

        println(s"Score: ${score}")
        println()

        score
    }

    def checkSyntax(line: String): SyntaxPairCount = {
        val syntaxItems = line.toList
        val syntaxPairCount = syntaxItems.foldLeft(new SyntaxPairCount())(addSyntaxItem)

        syntaxPairCount
    }

    def addSyntaxItem(syntaxCount: SyntaxPairCount, item: Char): SyntaxPairCount = {
        syntaxCount.count(item)
        syntaxCount
    }

    class SyntaxPairCount() {
        var stack = Stack[Int]()

        var illegalCounters: List[Int] = 0 :: 0 :: 0 :: 0 :: Nil
        
        def count(c: Char) : Unit = {
            c match {
                case '(' => inc(1)
                case '[' => inc(2)
                case '{' => inc(3)
                case '<' => inc(4)
                case ')' => dec(1)
                case ']' => dec(2)
                case '}' => dec(3)
                case '>' => dec(4)
            }
        }

        def inc(syntax: Int): Unit = {
            stack.push(syntax)
        }

        def dec(syntax: Int): Unit = {
            var illegal = false;

            if (stack.size == 0) {
                illegal = true
            } else {
                val popVal = stack.pop()
                if (popVal != syntax) {
                    illegal = true
                }
            }

            if (illegal) {
                countIllegal(syntax)
            }
        }

        def countIllegal(syntax: Int): Unit = {
            val index = syntax - 1
            val currentCount = illegalCounters(index)
            illegalCounters = illegalCounters.updated(index, currentCount + 1)
        }

        def toSyntaxText(syntax: Int, in: Boolean): String = {
            if (in) {
                syntax match {
                    case 1 => "("
                    case 2 => "["
                    case 3 => "{"
                    case 4 => "<"
                }
            }
            else {
                syntax match {
                    case 1 => ")"
                    case 2 => "]"
                    case 3 => "}"
                    case 4 => ">"
                }
            }
        }

        def score(syntax: Int): Int = {
            syntax match {
                    case 1 => 3
                    case 2 => 57
                    case 3 => 1197
                    case 4 => 25137
                }
        }

        def calcScore(): Long = {
            illegalCounters(0) * 3 + illegalCounters(1) * 57 + illegalCounters(2) * 1197 + illegalCounters(3) * 25137
        }       
    }
}