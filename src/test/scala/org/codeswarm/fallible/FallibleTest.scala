package org.codeswarm.fallible

import org.scalatest.FunSuite

class FallibleTest extends FunSuite {

  test("Success(_).toOption") { assert(Success(7).toOption == Some(7)) }
  test("Failure().toOption") { assert(Failure().toOption == None) }
  test("Failure(_).toOption") { assert(Failure(7).toOption == None) }

  test("Success(_).getErrors") { assert(Success(7).getErrors == List()) }
  test("Failure().getErrors") { assert(Failure().getErrors == List()) }
  test("Failure(_).getErrors") { assert(Failure(7).getErrors == List(7)) }
  test("Failure(_, _).getErrors") { assert(Failure(7, 9).getErrors == List(7, 9)) }

  test("Success(_).toEither") { assert(Success(7).toEither == Right(7)) }
  test("Failure().toEither") { assert(Failure().toEither == Left(List())) }
  test("Failure(_).toEither") { assert(Failure(7).toEither == Left(List(7))) }

  test("Success(_).map") { assert(Success(7).map { _ + 2 } == Success(9)) }
  test("Failure().map") { assert(Failure().map { x: Int => x + 2 } == Failure()) }
  test("Failure(_).map") { assert(Failure(7).map { x: Int => x + 2 } == Failure(7)) }

  def add3IfOdd(x: Int): Fallible[Int, String] = if (x % 2 == 1) Success(x+3) else Failure("%d is even".format(x))
  test("Success(_).flatMap => Success(_)") { assert(Success(7).flatMap(add3IfOdd) == Success(10)) }
  test("Success(_).flatMap => Failure(_)") { assert(Success(8).flatMap(add3IfOdd) == Failure("8 is even")) }
  test("Failure(_).flatMap => Success(_)") { assert(Failure(7).flatMap(_ => Success(5)) == Failure(7)) }
  test("Failure(_).flatMap => Failure(_)") { assert(Failure(7).flatMap(_ => Failure(2)) == Failure(7)) }

  test("Success(_) || two") { assert((Success(1) || Success(2)) == Success(1)) }
  test("Success(_) || three") { assert((Success(1) || Success(2) || Success(3)) == Success(1)) }
  test("Success(_) || four") { assert((Success(1) || Success(2) || Success(3) || Success(4)) == Success(1)) }
  test("Success(_) || Failure()") { assert((Success(1) || Failure()) == Success(1)) }
  test("Failure() || Success(_)") { assert((Failure() || Success(1)) == Success(1)) }
  test("Failure(_) || two") { assert((Failure(1) || Failure(2)) == Failure(1, 2)) }
  test("Failure(_) || three") { assert((Failure(1) || Failure(2) || Failure(3)) == Failure(1, 2, 3)) }
  test("Failure(_) || four") { assert((Failure(1) || Failure(2) || Failure(3) || Failure(4)) == Failure(1, 2, 3, 4)) }

  test("Success(_) && Success(_)") { assert((Success(1) && Success(2)) == Success((1, 2))) }
  test("Failure() && Success(_)") { assert((Failure() && Success(3)) == Failure()) }
  test("Success(_) && Failure(_, _)") { assert((Success(1) && Failure(3, 4)) == Failure(3, 4)) }
  test("Failure(_) && Success(_) && Failure(_, _)") { assert((Failure(2) && Success(1) && Failure(3, 4)) == Failure(2, 3, 4)) }
  test("Failure() && Failure()") { assert((Failure() && Failure()) == Failure()) }

}
