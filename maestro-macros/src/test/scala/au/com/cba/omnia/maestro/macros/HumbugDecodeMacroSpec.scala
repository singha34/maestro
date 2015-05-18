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

package au.com.cba.omnia.maestro.macros

import au.com.cba.omnia.maestro.core.codec._

import au.com.cba.omnia.maestro.test.Spec
import au.com.cba.omnia.maestro.test.Arbitraries._
import au.com.cba.omnia.maestro.test.thrift.humbug.{Types, Large}

object HumbugDecodeMacroSpec extends Spec { def is = s2"""

HumbugDecodeMacro
=================

  decode                           $decode
  parse empty strings              $empty
  type mismatch                    $typeError
  not enough fields                $sizeError
  decode very large thrift structs $large

"""

  implicit val encodeTypes = Macros.mkEncode[Types]
  implicit val decodeTypes = Macros.mkDecode[Types]
  implicit val encodeLarge = Macros.mkEncode[Large]
  implicit val decodeLarge = Macros.mkDecode[Large]

  val noneVal = "\u0000"

  def decode = prop { (types: Types) =>
    decodeTypes.decode(noneVal, Encode.encode(noneVal, types)) must_== DecodeOk(types)
  }

  def empty = {
    val data = List(
      "",
      "false",
      "1",
      "2",
      "3.0",
      "",
      ""
    )

    val expected = new Types()
    expected.stringField    = ""
    expected.booleanField   = false
    expected.intField       = 1
    expected.longField      = 2
    expected.doubleField    = 3.0
    expected.optIntField    = None
    expected.optStringField = Some("")

    decodeTypes.decode(noneVal, data) must_== DecodeOk(expected)
  }

  def typeError = {
    val encoded = List(
      "1", "2", "3", "4", "5", "6", "7"
    )
    decodeTypes.decode(noneVal, encoded) must parseErrorMatcher[Types](DecodeError(
      List("2", "3", "4", "5", "6", "7"),
      1,
      ParseError("2", "Boolean", null)
    ))
  }

  def sizeError = {
    decodeTypes.decode(noneVal, List()) must_== DecodeError(
      List(),
      0,
      NotEnoughInput(7, "au.com.cba.omnia.maestro.test.thrift.humbug.Types")
    )
  }

  def large = {
    val large = createLarge
    decodeLarge.decode(noneVal, Encode.encode(noneVal, large)) must_== DecodeOk(large)
  }

  def parseErrorMatcher[T] = (be_==(_: DecodeResult[T])) ^^^ ((_: DecodeResult[T]) match {
    case e@DecodeError(_, _, r@ParseError(_, _, _)) =>
      e.copy(reason = r.copy(error = null))
    case other => other
  })
}
