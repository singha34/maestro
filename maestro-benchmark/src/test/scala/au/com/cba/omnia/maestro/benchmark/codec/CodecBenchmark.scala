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

package au.com.cba.omnia.maestro.benchmark.codec

import org.scalameter.api.{PerformanceTest, Gen}

import au.com.cba.omnia.maestro.innercore.codec.{Encode, Decode}
import au.com.cba.omnia.maestro.macros.Macros

import au.com.cba.omnia.maestro.benchmark.thrift.{Struct10, Struct20, Struct30, Generators}
import au.com.cba.omnia.maestro.benchmark.thrift.Implicits._

object CodecBenchmark extends PerformanceTest.OfflineReport {
  def testDecode[A : Decode](gen: Gen[Array[List[String]]]) =
    using(gen) in { rows => {
      var i = 0
      while (i < rows.length) {
        Decode.decode[A]("", rows(i))
        i = i+1
      }
    }}

  def testEncode[A : Encode](gen: Gen[Array[A]]) =
    using(gen) in { values => {
      var i = 0
      while (i < values.length) {
        Encode.encode[A]("", values(i))
        i = i+1
      }
    }}

  performance of "Codecs" in {
    measure method "decode[Struct10]" in testDecode[Struct10](Generators.struct10Rows)
    measure method "decode[Struct20]" in testDecode[Struct20](Generators.struct20Rows)
    measure method "decode[Struct30]" in testDecode[Struct30](Generators.struct30Rows)

    measure method "encode[Struct10]" in testEncode[Struct10](Generators.struct10Values)
    measure method "encode[Struct20]" in testEncode[Struct20](Generators.struct20Values)
    measure method "encode[Struct30]" in testEncode[Struct30](Generators.struct30Values)
  }
}
