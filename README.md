Fallible
========

Example usage
-------------

```scala
import org.codeswarm.fallible._

def parse(s: String): Fallible[Int, String] =
  try {
    Success(s.toInt)
  } catch {
    case e: NumberFormatException =>
      Failure("Invalid format: '%s'".format(s))
  }

def halve(s: String): Fallible[Int, String] =
  parse(s) flatMap { i =>
    if (i % 2 == 0)
      Success(i / 2)
    else
      Failure("Odd number: %d".format(i))
  }

/* Result: Success(12)
 */
println(halve("24"))

/* Result: Failure(List(Odd number: 25))
 */
println(halve("25"))

/* Result: Failure(List(Invalid format: 'pie'))
 */
println(halve("pie"))

/* OR operator returns the first success encountered.
 *
 * Result: Success(4)
 */
println(halve("7") || halve("8") || halve("9") || halve("10"))

/* AND operator collects all failures.
 *
 * Result: Failure(List(Odd number: 7, Invalid format: pie))
 */
println(halve("7") && halve("8") && halve("pie") && halve("10"))

```
