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

package au.com.cba.omnia.maestro.innercore
package codec

import scalaz._, Scalaz._
import shapeless._

/**
  * Represents `A` as a list of string.
  *
  * None values are encoded using the provided string.
  */
case class Encode[A](run: (String, A) => List[String]) {
  /** Contramaps across the input.*/
  def contramap[B](f: B => A): Encode[B] =
    Encode[B]((none, a) => run(none, f(a)))
}

/** Encode companion object.*/
object Encode extends TypeClassCompanion[Encode] {
  /** Encodes `A` as a list of string. `None` are encoded as the supplied `none` string.*/
  def encode[A : Encode](none: String, a: A): List[String] =
    Encode.of[A].run(none, a)

  /** Gets the encoder for `A` provided an implicit encoder for `A` is in scope.*/
  def of[A : Encode]: Encode[A] =
    implicitly[Encode[A]]

  def value[A](run: A => String): Encode[A] =
    Encode[A]((none, a) => run(a) :: Nil)

  implicit val BooleanEncode: Encode[Boolean] =
    value(_.toString)

  implicit val IntEncode: Encode[Int] =
    value(_.toString)

  implicit val LongEncode: Encode[Long] =
    value(_.toString)

  implicit val DoubleEncode: Encode[Double] =
    value(_.toString)

  implicit val StringEncode: Encode[String] =
    value(_.toString)

  implicit def OptionEncode[A : Encode]: Encode[Option[A]] =
    Encode((none, a) => a.cata(
      encode[A](none, _),
      List(none)
    ))

  implicit def ListEncode[A : Encode]: Encode[List[A]] =
    Encode((none, as) => as.flatMap(encode[A](none, _)))

  implicit def VectorEncode[A : Encode]: Encode[Vector[A]] =
    Encode.of[List[A]].contramap(_.toList)

  implicit def ProductEncode[A]: Encode[A] =
    macro TypeClass.derive_impl[Encode, A]

  implicit def EncodeTypeClass: ProductTypeClass[Encode] = new ProductTypeClass[Encode] {
    def emptyProduct =
      Encode((_, _) => List())

    def product[A, T <: HList](A: Encode[A], T: Encode[T]) =
      Encode((none, a) => A.run(none, a.head) ++ T.run(none, a.tail))

    def project[F, G](instance: => Encode[G], to : F => G, from : G => F) =
      instance.contramap(to)
  }

  implicit def EncodeContravariant: Contravariant[Encode] = new  Contravariant[Encode] {
    def contramap[A, B](r: Encode[A])(f: B => A) = r contramap f
  }
}
