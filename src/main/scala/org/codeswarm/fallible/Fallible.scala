package org.codeswarm.fallible

/** Represents a value or a list of errors explaining why the value is absent.
  * Instances of `Fallible[A, E]` are an instance of either `Success[A]` or `Failure[E]`.
  */
sealed abstract class Fallible[+A, +E] {

  /** Returns true if this is an instance of `Success`, or false otherwise.
    */
  def isSuccess: Boolean =
    this match {
      case Success(_) => true
      case Failure(_) => false
    }

  /** Returns true if this is an instance of `Failure`, or false otherwise.
    */
  def isFailure: Boolean = !isSuccess

  /** Returns `Some` if this is an instance of `Success`, or `None` otherwise.
    */
  def toOption: Option[A] =
    this match {
      case Success(x) => Some(x)
      case Failure(_) => None
    }

  /** Returns all errors.
    *
    * If this is an instance of `Success`, returns an empty list, An instance of
    * `Failure` may also return an empty list, denoting failure without any explanation.
    */
  def getErrors: List[E] =
    this match {
      case Success(_) => List()
      case Failure(e) => e
    }

  /** Returns `Right` if this is an instance of `Success`, or `Left` otherwise.
    */
  def toEither: Either[List[E], A] =
    this match {
      case Success(x) => Right(x)
      case Failure(e) => Left(e)
    }

  /** Returns the result of applying `f` to this object's value if this is an instance of `Success`.
    */
  def map[B, E1 >: E](f: A => B): Fallible[B, E1] =
    this match {
      case Success(x) => Success(f(x))
      case Failure(e) => Failure(e)
    }

  /** Returns the result of applying a fallible transformation `f` to this
    * object's value if this is an instance of success.
    *
    * The resulting object will be an instance of `Success` if and only if
    * both this and the transformation result are instances of `Success`.
    */
  def flatMap[B, E1 >: E](f: A => Fallible[B, E1]): Fallible[B, E1] =
    this match {
      case Success(x) => f(x)
      case Failure(e) => Failure(e)
    }

  /** Returns the result of applying `f` to each error.
    */
  def mapError[E1](f: E => E1): Fallible[A, E1] =
    this match {
      case Success(x) => Success(x)
      case Failure(e) => Failure(e map f)
    }

  /** Returns the first `Success` among `this` and `that`, or `Failure` if both are failures.
    *
    * This is somewhat analogous to a Boolean '''or''' operation, because the result
    * is `Success` if and only if `this` '''or''' `that` is `Success`.
    *
    * If both `this` and `that` are instances of `Failure`, then the result contains
    * the errors from both `Failures`.
    *
    * This method may be useful if you need to select a single value
    * from a sequence of fallible fallbacks.
    */
  def || [A1 >: A, E1 >: E](that: Fallible[A1, E1]): Fallible[A1, E1] =
    ( this, that ) match {
      case ( Success(x), _ ) => Success(x)
      case ( Failure(_), Success(x) ) => Success(x)
      case ( Failure(e1), Failure(e2) ) => Failure(e1 ++ e2)
    }

  /** Returns `Success` containing a tuple of `this` value and `that` value if '''both'''
    * `this` and `that` are instances of `Success`, or `Failure` if either is an instance of `Failure`.
    *
    * This is somewhat analogous to a Boolean '''and''' operation, because the result is `Success`
    * if and only if `this` '''and''' `that` are `Success`.
    */
  def && [B, E1 >: E](that: Fallible[B, E1]): Fallible[(A, B), E1] =
    ( this, that ) match {
      case ( Success(x1), Success(x2) ) => Success((x1, x2))
      case ( a, b ) => Failure(List(a, b).map(_.getErrors).flatten)
    }

}

object Fallible {

  /** Constructs a `Fallable` from an `Option` by mapping
    * `Some(x)` to `Success(x)` and `None` to `Failure`.
    * If the result is `Failure`, its list of errors is empty.
    */
  def apply[A](o: Option[A]): Fallible[A, Nothing] =
    o match {
      case Some(x) => Success(x)
      case None => Failure()
    }

  /** Constructs a `Fallable` from an `Option` by mapping
    * `Some(x)` to `Success(x)` and `None` to `Failure`.
    * If the result is `Failure`, it contains a single error provided by `e`.
    */
  def apply[A, E](o: Option[A], e: => E): Fallible[A, E] =
    o match {
      case Some(x) => Success(x)
      case None => Failure(e)
    }

}

/** Class `Success[A]` represents existing values of type `A`.
  */
case class Success[+A](value: A) extends Fallible[A, Nothing]

/** Class `Failure[E]` represents a list of errors of type `E`.
  */
case class Failure[+E](errors: List[E]) extends Fallible[Nothing, E]

object Failure {

  def apply[A, E](errors: E*): Failure[E] = Failure(errors.toList)

}
