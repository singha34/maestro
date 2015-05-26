package au.com.cba.omnia.maestro.macros

import au.com.cba.omnia.maestro.test.Spec
import au.com.cba.omnia.maestro.test.thrift.humbug._
import au.com.cba.omnia.maestro.test.thrift.scrooge._

object JoinMacroSpec extends Spec { def is = s2"""
JoinMacroSpec
==================
  Humbug generated Thrift structures
    can be joined                                                                                  $joinHumbug
    can handle multiple equal input fields                                                         $multipleEqualSourcesHumbug
    throws exception when encountering multiple differing input fields                             $multipleDifferingSourcesHumbug

  Scrooge generated Thrift structures
    can be joined                                                                                  $joinScrooge
    can handle multiple equal input fields                                                         $multipleEqualSourcesScrooge
    throws exception when encountering multiple differing input fields                             $multipleDifferingSourcesScrooge

  All Thrift structures
    will NOT compile when output field has matching input field with incompatible type             $incompatibleTypes
    will NOT compile when output thrift structure have fields that are not in the input structures $joinWithMissingField
    will NOT compile when provided with tuple containing non-Thrift structures                     $notThriftError

  Combined Humbug and Scrooge Thrift structures
    can join Scrooge and Humbug to Humbug                                                          $mixedToHumbug
    can join Scrooge and Humbug to Scrooge                                                         $mixedToScrooge
"""

  val imports =
    """import au.com.cba.omnia.maestro.macros._
       import au.com.cba.omnia.maestro.test.thrift.humbug._
       import au.com.cba.omnia.maestro.test.thrift.scrooge._
    """

  val scroogeStructOne = JoinOneScrooge("some-string", true, 42)
  val scroogeStructTwo = JoinTwoScrooge("some-string", 1000l, 2d)

  val scroogeDuplicateOk  = JoinOneDuplicateScrooge(42)
  val scroogeDuplicateBad = JoinOneDuplicateScrooge(13)

  val scroogeIncompatible = JoinOneIncompatibleScrooge("not int")

  val humbugStructOne = {
    val one = new JoinOneHumbug
    one.stringField  = "some-string"
    one.booleanField = true
    one.someField    = 42
    one
  }

  val humbugStructTwo = {
    val two = new JoinTwoHumbug
    two.stringField = "some-string"
    two.longField   = 1000l
    two.doubleField = 2d
    two
  }

  val humbugDuplicateOk  = {
    val dup = new JoinOneDuplicateHumbug
    dup.someField = 42

    dup
  }

  val humbugDuplicateBad = {
    val dup = new JoinOneDuplicateHumbug
    dup.someField = 13
    dup
  }


  def joinHumbug = {
    val join = Macros.mkJoin[(JoinOneHumbug, JoinTwoHumbug), JoinableHumbug]

    val resultJoined = new JoinableHumbug
    resultJoined.booleanField = true
    resultJoined.longField    = 1000l
    resultJoined.someField    = 42
    join.run((humbugStructOne, humbugStructTwo)) must beEqualTo(resultJoined)
  }

  def joinScrooge = {
    val join = Macros.mkJoin[(JoinOneScrooge, JoinTwoScrooge), JoinableScrooge]

    val resultJoined = JoinableScrooge(true, 1000, 42)
    join.run((scroogeStructOne, scroogeStructTwo)) must beEqualTo(resultJoined)
  }

  def multipleEqualSourcesHumbug = {
    val join = Macros.mkJoin[(JoinOneHumbug, JoinTwoHumbug, JoinOneDuplicateHumbug), JoinableHumbug]

    val resultJoined          = new JoinableHumbug
    resultJoined.booleanField = true
    resultJoined.longField    = 1000l
    resultJoined.someField    = 42
    join.run((humbugStructOne, humbugStructTwo, humbugDuplicateOk)) must_== resultJoined
  }

  def multipleEqualSourcesScrooge = {
    val join = Macros.mkJoin[(JoinOneScrooge, JoinTwoScrooge, JoinOneDuplicateScrooge), JoinableScrooge]

    val resultJoined = JoinableScrooge(true, 1000l, 42)
    join.run((scroogeStructOne, scroogeStructTwo, scroogeDuplicateOk)) must_== resultJoined
  }

  def multipleDifferingSourcesHumbug = {
    val join = Macros.mkJoin[(JoinOneHumbug, JoinTwoHumbug, JoinOneDuplicateHumbug), JoinableHumbug]

    join.run((humbugStructOne, humbugStructTwo, humbugDuplicateBad)) must throwAn[IllegalArgumentException]
  }

  def multipleDifferingSourcesScrooge = {
    val join = Macros.mkJoin[(JoinOneScrooge, JoinTwoScrooge, JoinOneDuplicateScrooge), JoinableScrooge]

    join.run((scroogeStructOne, scroogeStructTwo, scroogeDuplicateBad)) must throwAn[IllegalArgumentException]
  }

  def incompatibleTypes = {
    val compileErrors = MacroUtils.compileErrors("Macros.mkJoin[(JoinOneScrooge, JoinTwoScrooge, JoinOneIncompatibleScrooge), JoinableScrooge]")

    compileErrors must beSome("Join requires output fields to have matching compatible input fields: someField is not compatible with JoinOneIncompatibleScrooge.someField: => String")
  }

  def joinWithMissingField = {
    val compileErrors = MacroUtils.compileErrors("Macros.mkJoin[(JoinOneScrooge, JoinTwoScrooge), UnjoinableScrooge]")

    compileErrors must beSome("Join requires output fields to have matching compatible input fields: missingField has no input field")
  }

  def notThriftError = {
    val compileErrors = MacroUtils.compileErrors("Macros.mkJoin[(Int, Int, JoinOneHumbug), SubTwo]")

    compileErrors must beSome("Join requires a product of thrift structs: Int is not a thrift struct, Int is not a thrift struct")
  }

  def mixedToHumbug = {
    val join = Macros.mkJoin[(JoinOneScrooge, JoinTwoHumbug), JoinableHumbug]

    val resultJoined = new JoinableHumbug
    resultJoined.booleanField = true
    resultJoined.longField    = 1000l
    resultJoined.someField    = 42
    join.run((scroogeStructOne, humbugStructTwo)) must beEqualTo(resultJoined)
  }

  def mixedToScrooge = {
    val join = Macros.mkJoin[(JoinOneHumbug, JoinTwoScrooge), JoinableScrooge]

    val resultJoined = JoinableScrooge(true, 1000, 42)
    join.run((humbugStructOne, scroogeStructTwo)) must beEqualTo(resultJoined)
  }
}
