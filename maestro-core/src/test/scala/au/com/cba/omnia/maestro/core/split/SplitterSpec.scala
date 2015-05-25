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

package au.com.cba.omnia.maestro.core
package split

import org.scalacheck.{Gen, Prop}

import au.com.cba.omnia.maestro.core.split
import au.com.cba.omnia.maestro.innercore.test.Spec

object SplitterSpec extends Spec { def is = s2"""

Splitter properties
====================

  delimited splitter segments = numDelims + 1                    $delimitedNum
  delimited splitter can do roundtrip                            $delimitedRoundtrip

  csv splitter can parse sample row                              $csvSampleRow
  csv splitter should not fail with even quotes                  $csvSmokeTest
  csv splitter can roundtrip w/o quotes                          $csvRoundtripNoQuotes
  csv splitter roundtrip w/ fully quoted fields == remove quotes $csvRoundtripFullyQuoted
  csv splitter gives correct number of segments                  $csvNum

  fixed splitter returns bad length on bad input                 $fixedBadLength
  fixed splitter length is correct on good inputs                $fixedNum
  fixed splitter can do roundtrip on good inputs                 $fixedRoundtrip
"""

  val delimiterChr: Char = '$' // delimitedStr assumes this is not an alphaChar
  val delimiter: String = delimiterChr.toString

  // generators

  // no idea what the field count or even character count will be,
  // but it should be valid csv if the number of quotes is even
  val goodCsv: Gen[String] =
    Gen.listOf(Gen.frequency(
      (8, Gen.alphaNumChar),
      (1, Gen.const('"')),
      (1, Gen.const(delimiterChr))
    ))
      .filter(chars => chars.count(_ == '"') % 2 == 0)
      .map(_.mkString)

  val fullyQuotedField: Gen[String] =
    Gen.listOf(Gen.frequency(
      (9, Gen.alphaNumChar),
      (1, Gen.const(delimiterChr))
    )).map(_.mkString("\"", "", "\""))

  val fullyQuotedCsv: Gen[(Int, String)] = for {
    numSegments <- Gen.choose(0,20)
    segments    <- Gen.listOfN(numSegments, Gen.oneOf(
                     Gen.alphaStr,
                     fullyQuotedField
                   ))
  } yield (numSegments, segments.mkString(delimiter))

  val delimitedStr: Gen[String] = for {
    numSegments <- Gen.choose(0,20)
    segments    <- Gen.listOfN(numSegments, Gen.alphaStr)
  } yield segments.mkString(delimiter)

  val goodFixedLengthStr: Gen[(List[Int], String)] = for {
    numFields <- Gen.choose(0, 100)
    lengths   <- Gen.listOfN(numFields, Gen.choose(0, 20))
    row       <- Gen.listOfN(lengths.sum, Gen.alphaChar).map(_.mkString)
  } yield (lengths, row)

  val badFixedLengthStr: Gen[(List[Int], String)] = for {
    numFields <- Gen.choose(0, 100)
    lengths   <- Gen.listOfN(numFields, Gen.choose(0, 20))
    offset    <- Gen.choose(-10,10).suchThat(off => off != 0 && lengths.sum + off >= 0)
    row       <- Gen.listOfN(lengths.sum + offset, Gen.alphaChar).map(_.mkString)
  } yield (lengths, row)

  // properties

  val delimitedNum: Prop = Prop.forAll(delimitedStr) (str => {
    val numDelimiters = str.count(_ == delimiterChr)
    val segments      = Splitter.delimited(delimiter).run(str)
    segments.length must_== (numDelimiters+1)
  })

  val delimitedRoundtrip: Prop = Prop.forAll(delimitedStr) (str => {
    val segments = Splitter.delimited(delimiter).run(str)
    segments.mkString(delimiter) must_== str
  })

  val csvSampleRow = {
    val sampleRow = """"ABC001","232",45,0.00,"Sample Data","Sample Data (String,Title)","8/298/78899",1,"26/11/2014",1,"","""""
    val expected = List("ABC001","232","45","0.00","Sample Data","Sample Data (String,Title)","8/298/78899","1","26/11/2014","1","","")
    Splitter.csv(',').run(sampleRow) must_== expected
  }

  val csvSmokeTest: Prop = Prop.forAll(goodCsv) (str =>
    Splitter.csv(delimiterChr).run(str) must beLike { case _ : List[String] => ok }
  )

  val csvRoundtripNoQuotes: Prop = Prop.forAll(delimitedStr) (str => {
    val segments = Splitter.csv(delimiterChr).run(str)
    segments.mkString(delimiter) must_== str
  })

  val csvRoundtripFullyQuoted: Prop = Prop.forAll(fullyQuotedCsv) { case (_, str) => {
    val segments = Splitter.csv(delimiterChr).run(str)
    segments.mkString(delimiter) must_== str.replaceAll("\"", "")
  }}

  val csvNum = Prop.forAll(fullyQuotedCsv) { case (numSegments, str) => {
    val segments = Splitter.csv(delimiterChr).run(str)
    // csv reasonably assumes one empty segment rather than 0 segments
    segments.length must_== Math.max(numSegments,1)
  }}


  val fixedBadLength: Prop = Prop.forAll(badFixedLengthStr) { case (lengths, str) => {
    val segments = Splitter.fixed(lengths).run(str)
    segments.length must_!= lengths.length
  }}

  val fixedNum: Prop = Prop.forAll(goodFixedLengthStr) { case (lengths, str) => {
    val segments = Splitter.fixed(lengths).run(str)
    segments.length must_== lengths.length
  }}

  val fixedRoundtrip: Prop = Prop.forAll(goodFixedLengthStr) { case (lengths, str) => {
    val segments = Splitter.fixed(lengths).run(str)
    segments.mkString must_== str
  }}
}
