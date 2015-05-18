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

package au.com.cba.omnia.maestro.core.test

import scalaz._, Scalaz._, \&/.These

import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

import org.scalacheck.{Arbitrary, Gen}, Arbitrary.arbitrary

import au.com.cba.omnia.maestro.core.codec._

object Arbitraries {
  implicit def ReasonArbitrary: Arbitrary[Reason] =
    Arbitrary(Gen.oneOf(
      (arbitrary[String] |@| arbitrary[String] |@| arbitrary[These[String, Throwable]])(ParseError.apply)
    , (arbitrary[Int] |@| arbitrary[String])(NotEnoughInput)
    , Gen.const(TooMuchInput)
    ))

  implicit def DecodeResultArbitrary[A: Arbitrary]: Arbitrary[DecodeResult[A]] =
    Arbitrary(Gen.frequency(
      (4, arbitrary[A] map DecodeResult.ok)
    , (1, (arbitrary[List[String]] |@| arbitrary[Int] |@| arbitrary[Reason])((ss, n, reason) => DecodeError(ss, n, reason)))
    ))


  implicit def EncodeIntArbitrary: Arbitrary[Encode[Int]] =
    Arbitrary(Gen.oneOf(Encode.IntEncode :: Nil))

  implicit def DecodeIntArbitrary: Arbitrary[Decode[Int]] =
    Arbitrary(Gen.oneOf(Decode.IntDecode :: Nil))

  implicit def DecodeIntIntArbitrary: Arbitrary[Decode[Int => Int]] =
    Arbitrary(arbitrary[Int => Int] map (_.point[Decode]))
}
