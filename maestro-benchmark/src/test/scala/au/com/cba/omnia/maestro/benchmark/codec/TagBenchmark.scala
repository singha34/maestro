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

import au.com.cba.omnia.maestro.innercore.codec.Tag
import au.com.cba.omnia.maestro.macros.Macros

import au.com.cba.omnia.maestro.benchmark.thrift.{Struct10, Struct20, Struct30, Generators}
import au.com.cba.omnia.maestro.benchmark.thrift.Implicits._

object TagBenchmark extends PerformanceTest.OfflineReport {
  def testTag[A : Tag](gen: Gen[Array[List[String]]]) =
    using(gen) in { rows => {
      var i = 0
      while (i < rows.length) {
        Tag.tag[A](rows(i))
        i = i+1
      }
    }}

  performance of "Tag" in {
    measure method "tag[Struct10]" in testTag[Struct10](Generators.struct10Rows)
    measure method "tag[Struct20]" in testTag[Struct20](Generators.struct20Rows)
    measure method "tag[Struct30]" in testTag[Struct30](Generators.struct30Rows)
  }
}
