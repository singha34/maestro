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

package au.com.cba.omnia.maestro.core.task

import au.com.cba.omnia.maestro.innercore.data.Field
import au.com.cba.omnia.maestro.innercore.codec.{Decode, Tag}

import au.com.cba.omnia.maestro.innercore.thrift.scrooge.StringTriple

// Moved fields into a separate object to avoid serialisation issues.
object StringTripleFields {
  val fields: List[Field[StringTriple, _]] = List(
    Field("first" , (p: StringTriple) => p.first),
    Field("second", (p: StringTriple) => p.second),
    Field("date"  , (p: StringTriple) => p.date)
  )
}

trait StringTripleSupport {
  implicit val StringTripleDecode: Decode[StringTriple] = for {
    first  <- Decode.of[String]
    second <- Decode.of[String]
    date   <- Decode.of[String]
  } yield StringTriple(first, second, date)

  implicit val StringTripleTag: Tag[StringTriple] = {
    Tag.fromFields(StringTripleFields.fields)
  }
}
