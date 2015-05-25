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

package au.com.cba.omnia.maestro.benchmark.thrift

import au.com.cba.omnia.maestro.innercore.codec.{Encode, Decode, Tag}
import au.com.cba.omnia.maestro.macros.Macros

/** Contains implicits for thrift types */
object Implicits extends Serializable {
  implicit val Struct10Decode: Decode[Struct10] =
    Macros.mkDecode[Struct10]

  implicit val Struct20Decode: Decode[Struct20] =
    Macros.mkDecode[Struct20]

  implicit val Struct30Decode: Decode[Struct30] =
    Macros.mkDecode[Struct30]

  implicit val Struct10Encode: Encode[Struct10] =
    Macros.mkEncode[Struct10]

  implicit val Struct20Encode: Encode[Struct20] =
    Macros.mkEncode[Struct20]

  implicit val Struct30Encode: Encode[Struct30] =
    Macros.mkEncode[Struct30]

  implicit val Struct10Tag: Tag[Struct10] =
    Macros.mkTag[Struct10]

  implicit val Struct20Tag: Tag[Struct20] =
    Macros.mkTag[Struct20]

  implicit val Struct30Tag: Tag[Struct30] =
    Macros.mkTag[Struct30]
}
