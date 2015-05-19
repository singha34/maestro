//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.maestro.core.codec

import scala.util.control.NonFatal
import scala.reflect.runtime.universe.{typeOf, TypeTag, WeakTypeTag}

import scalaz._, Scalaz._, \&/._

import shapeless.{ProductTypeClass, ProductTypeClassCompanion}

/**
  * Decoder for a list of strings.
  *
  * It also keeps track of the current position in the decode process.
  * For optional fields if they have the supplied none value they are decoded as `None`.
  * Empty strings are also decoded as None for optional fields except for optional string fields.
  */
case class Decode[A](run: (String, List[String], Int) => DecodeResult[(List[String], Int, A)]) {
  /**
    * Decodes the supplied list of strings.
    *
    * For optional fields if they have the supplied none value they are decoded as `None`.
    * Empty strings are also decoded as None for optional fields except for optional string fields.
    */
  def decode(none: String, source: List[String]): DecodeResult[A] =
    run(none, source, 0).flatMap({
      case (Nil, _, a)             => DecodeOk(a)
      case (remainder, counter, a) => DecodeError(remainder, counter, TooMuchInput)
    })

  /** Maps across the decoded value.*/
  def map[B](f: A => B): Decode[B] =
    flatMap(f andThen Decode.strict[B])

  /** Flat maps across the decoded value.*/
  def flatMap[B](f: A => Decode[B]): Decode[B] =
    Decode((none, source, start) => run(none, source, start).flatMap({
      case (remainder, counter, value) => f(value).run(none, remainder, counter)
    }))
}

/** Encode companion object.*/
object Decode extends ProductTypeClassCompanion[Decode] {
  /** Encodes `A` as a list of string. `None` are replaced with the supplied `none` string.*/
  def decode[A: Decode](none: String, source: List[String]): DecodeResult[A] =
    Decode.of[A].decode(none, source)

  /** Gets the encoder for `A` provided an implicit encoder for `A` is in scope.*/
  def of[A: Decode]: Decode[A] =
    implicitly[Decode[A]]

  /** Creates a Decode that will return `a`.*/
  def ok[A](a: => A): Decode[A] =
    Decode((none, source, n) => DecodeOk((source, n, a)))

  /**
    * Creates a Decode that will return `a`
    *
    * Unlike `ok` this does not use call by name.
    */
  def strict[A](a: A): Decode[A] =
    ok(a)

  /** Creates a Decode that will produce an error with the specified reason.*/
  def error[A](reason: Reason): Decode[A] =
    Decode((none, source, n) => DecodeError(source, n, reason))

  implicit val BooleanDecode: Decode[Boolean] =
    one("Boolean", s => unsafe(s.toBoolean))

  implicit val IntDecode: Decode[Int] =
    one("Int", s => unsafe(s.toInt))

  implicit val LongDecode: Decode[Long] =
    one("Long", s => unsafe(s.toLong))

  implicit val DoubleDecode: Decode[Double] =
    one("Double", s => unsafe(s.toDouble))

  implicit val StringDecode: Decode[String] =
    one("String", _.right)

  /** Decoder into Option[A]. '''NB: For strings "" is decoded as Some("")'''. */
  implicit def OptionDecode[A : TypeTag](implicit decode: Decode[A]):Decode[Option[A]] =
    Decode((none, source, n) => source match {
      case s :: remainder =>
        if ((s.isEmpty && !(typeOf[A] <:< typeOf[String])) || s == none)
          DecodeOk((remainder, n + 1 , None))
        else
          decode.map(Option(_)).run(none, source, n)
      case _ => DecodeError(source, n, NotEnoughInput(1, "option"))
    })

  implicit def ListDecode[A: Decode]: Decode[List[A]] =
    VectorDecode[A].map(_.toList)

  implicit def VectorDecode[A: Decode]: Decode[Vector[A]] =
    Decode((none, source, n) => {
      def loop(acc: DecodeResult[Vector[A]], source: List[String], n: Int): DecodeResult[(List[String], Int, Vector[A])] =
        acc.flatMap(xs =>
          Decode.of[A].run(none, source, n) match {
            case DecodeOk((remainder, counter, value)) =>
              loop(DecodeOk((xs :+ value)), remainder, counter)
            case DecodeError(_, _, _) =>
              acc.map((source, n, _))
          })
      loop(DecodeOk(Vector()), source, n)
    })

  val typeClass: ProductTypeClass[Decode] = new ProductTypeClass[Decode] {
    import shapeless._

    def emptyProduct =
      strict(HNil)

    def product[A, T <: HList](A: Decode[A], T: Decode[T]) =
      Decode((none, source, n) =>
        A.run(none, source, n) match {
          case DecodeOk((remainder, counter, a)) =>
            T.run(none, remainder, counter).map(_.map(a :: _))
          case DecodeError(remainder, counter, reason) =>
            DecodeError(remainder, counter, reason)
        })

    def project[F, G](instance: => Decode[G], to : F => G, from : G => F) =
      instance.map(from)
  }

  implicit def DecodeMonad: Monad[Decode] = new Monad[Decode] {
    def point[A](a: => A) = Decode((none, source, n) => DecodeOk((source, n, a)))
    def bind[A, B](m: Decode[A])(f: A => Decode[B]) = m flatMap f
  }

  def unsafe[A](a: => A): These[String, Throwable] \/ A =
    try a.right
    catch { case NonFatal(e) => That(e).left[A] }

  def one[A](tag: String, decode: String => These[String, Throwable] \/ A): Decode[A] =
    Decode((none, source, n) => source match {
      case s :: remainder =>
        decode(s) match {
          case -\/(err) =>
            DecodeError(remainder, n, ParseError(s, tag, err))
          case \/-(a) =>
            DecodeOk((remainder, n + 1, a))
        }
      case _ =>
        DecodeError(source, n, NotEnoughInput(1, tag))
    })
}
