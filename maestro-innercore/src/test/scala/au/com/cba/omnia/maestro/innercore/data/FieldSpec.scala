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
package data

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import com.twitter.scrooge.ThriftStruct

import au.com.cba.omnia.maestro.innercore.test.Spec
import au.com.cba.omnia.maestro.innercore.thrift.scrooge.StringPair

object FieldSpec extends Spec { def is = s2"""

Field equality
===========

Fields with
  different names should NOT be equal                                     $fieldNameNotEqual
  same names but different Thrift struct type should NOT be equal         $fieldDiffStructTypes
  same names but different return type should throw an Exception          $fieldDiffReturnTypes
  same names and Thrift struct should be equal and have equal hashCode    $fieldNameAndTypeEqual
"""
  val genDifferentString = for {
    string1 <- Gen.alphaStr
    string2 <- Gen.alphaStr
    if (string1 != string2)
  } yield (string1 ,string2)

  def fieldNameNotEqual = forAll(genDifferentString) {
    case (name1, name2) => {
      val fun    = (p:StringPair) => p.first
      val field1 = Field(name1, fun)
      val field2 = Field(name2, fun)

      field1 must_!= field2
    }
  }

  def fieldNameAndTypeEqual = forAll { (name: String, f: StringPair => String) => {
      val field1 = Field(name, f)
      val filed2 = Field(name, f)

      field1          must_== filed2
      field1.hashCode must_== filed2.hashCode
    }
  }

  def fieldDiffStructTypes = {
    val name   = "id"
    val field1 = Field(name, (x: StringPair)  => 1)
    val field2 = Field(name, (x: Int)         => 1)

    field1          must_!= field2
    field1.hashCode must_!= field2.hashCode
  }

  def fieldDiffReturnTypes = {
    val name   = "id"
    val field1 = Field(name, (x: ThriftStruct) => "")
    val field2 = Field(name, (x: ThriftStruct) => 1)

    def notValidEqualityCheck = { field1 == field2 }

    notValidEqualityCheck must throwA(new RuntimeException("Can't have two columns with the same name from the same Thrift structure with different column type"))

  }
}
