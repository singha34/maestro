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
package validate

import scala.util.control.NonFatal

import scalaz._, Scalaz._

import org.apache.commons.validator.routines._
import org.apache.commons.validator.GenericValidator

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, DateTimeFormat}

import au.com.cba.omnia.omnitool.{Result, Error}

import au.com.cba.omnia.maestro.innercore.data.Field

/**
  * Validator to check that the record conforms to expectations.
  *
  * Use this to error out records that don't meet expectations for example
  * checking that records all have a positive balance, etc.
  */
case class Validator[A](run: A => Result[A])

/** Some helpful functions to create [[Validator]]. */
object Validator {
  /** Combines all the provided validators. Running one after the other. */
  def all[A](validators: Validator[A]*): Validator[A] =
    Validator(a => ValidatorImpl.combineErrors(validators.map(_.run(a))).as(a))

  /** Creates a [[Validator]] from a predicate and error message. */
  def by[A](validation: A => Boolean, message: String): Validator[A] =
    Validator(a => if (validation(a)) Result.ok(a) else Result.fail(message))

  /** Creates a [[Validator]] from a [[Validator]] for a specific field. */
  def of[A, B](field: Field[A, B], check: Validator[B]): Validator[A] =
    Validator(a => check.run(field.get(a)).as(a))

  /** Creates a [[Validator]] that is always successful. */
  def pass[A] = Validator[A](a => Result.ok(a))
}

object Check {
  def oneOf(categories: String*): Validator[String] =
    Validator(s =>
      if (categories.contains(s)) Result.ok(s)
      else                        Result.fail(s"""Expected one of [${categories.mkString("|")}] but got ${s}.""")
    )

  def nonempty: Validator[String] =
    Validator(s => if (!s.isEmpty) Result.ok(s) else Result.fail("Value can not be empty."))

  def isDate(pattern: String): Validator[String] = {
    lazy val formatter = DateTimeFormat.forPattern(pattern)
    Validator(s =>
      try {
        val date: DateTime = formatter.parseDateTime(s)
        Result.ok(s)
      } catch {
           case NonFatal(e) => Result.fail(s"Date $s is not in the format $pattern")
      }
    )
  }

  def isEmail: Validator[String]=
    Validator(s => if (GenericValidator.isEmail(s)) Result.ok(s) else Result.fail(s"Data $s not valid email"))

  def isDomain: Validator[String]=
    Validator(s => if (DomainValidator.getInstance().isValid(s)) Result.ok(s) else Result.fail(s"Data $s not valid Domain"))

  def isIP: Validator[String]=
    Validator(s => if (InetAddressValidator.getInstance().isValid(s)) Result.ok(s) else Result.fail(s"Data $s not valid IP"))

  def isURL: Validator[String]=
    Validator(s => if (GenericValidator.isUrl(s)) Result.ok(s) else Result.fail(s"Data $s not valid URL"))
}

/**
  * WARNING: not considered part of the validator api,
  * We may change this without considering backwards compatibility.
  */
object ValidatorImpl {
  // squashes any errors in multiple results down to one error
  // has no idea how to combine A's, so just produces unit
  def combineErrors[A](results: Seq[Result[A]]): Result[Unit] = {
    val msgs = results collect {
      case Error(err) => err.fold(identity, _.toString, { case (msg, ex) => s"$msg: $ex"})
    }
    if (msgs.isEmpty) Result.ok(()) else Result.fail(msgs.mkString(", "))
  }
}
