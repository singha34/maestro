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
import au.com.cba.omnia.maestro.test.thrift.scrooge.Types

object ScroogeDecodeMacroSpec extends Spec { def is = s2"""

ScroogeDecodeMacro
===========

  decode              $decode
  parse empty strings $empty
  type mismatch       $typeError
  not enough fields   $sizeError

"""

  implicit val encodeTypes = Macros.mkEncode[Types]
  implicit val decodeTypes = Macros.mkDecode[Types]

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

    val expected = Types(
      "",
      false,
      1,
      2,
      3.0,
      None,
      Some("")
    )

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
      NotEnoughInput(7, "au.com.cba.omnia.maestro.test.thrift.scrooge.Types")
    )
  }

  def parseErrorMatcher[T] = (be_==(_: DecodeResult[T])) ^^^ ((_: DecodeResult[T]) match {
    case e@DecodeError(_, _, r@ParseError(_, _, _)) =>
      e.copy(reason = r.copy(error = null))
    case other => other
  })
}
