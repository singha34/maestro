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

package au.com.cba.omnia.maestro.core.scalding

import scala.util.{Success, Failure}

import scalaz._, Scalaz._

import com.twitter.scalding.Execution

import org.scalacheck.Arbitrary, Arbitrary._

import org.specs2.matcher.Matcher

import au.com.cba.omnia.thermometer.hive.ThermometerHiveSpec

import au.com.cba.omnia.omnitool.Result
import au.com.cba.omnia.omnitool.test.OmnitoolProperties.resultantMonad
import au.com.cba.omnia.omnitool.test.Arbitraries._

import au.com.cba.omnia.permafrost.hdfs.Hdfs

import au.com.cba.omnia.ebenezer.scrooge.hive.Hive

import ExecutionOps._

object Executions {
  def hive   = Execution.fromHive(Hive.value(throw new Exception("test")))
  def hdfs   = Execution.fromHdfs(Hdfs.value(throw new Exception("test")))
  def result = Execution.fromResult(Result.fail("error"))
  def either = Execution.fromEither("error".left)
}

object RichExecutionSpec extends ThermometerHiveSpec { def is = s2"""

Rich Execution
==============

Execution operations should:
  obey resultant monad laws (monad and plus laws)       ${resultantMonad.laws[Execution]}

The RichExecution object should:
  provide useful exception information for `fromHive`   $hive
  provide useful exception information for `fromHdfs`   $hdfs
  provide useful exception information for `fromResult` $result
  provide useful exception information for `fromEither` $either

"""

  def hive   = Executions.hive   must beFailureWithClass(Executions)
  def hdfs   = Executions.hdfs   must beFailureWithClass(Executions)
  def result = Executions.result must beFailureWithClass(Executions)
  def either = Executions.either must beFailureWithClass(Executions, 2)

  def beFailureWithClass[A](clazz: Any, skip: Int = 1): Matcher[Execution[A]] =
    (execution: Execution[A]) => execute(execution) must beLike {
      case Failure(t) => t.getStackTrace()(skip).getClassName must_== clazz.getClass.getName
    }

  implicit def ExecutionAribtrary[A : Arbitrary]: Arbitrary[Execution[A]] =
    Arbitrary(arbitrary[Either[Result[A], Either[Throwable, A]]].map {
      case Left(r)         => Execution.fromResult(r)
      case Right(Left(ex)) => Execution.from(throw ex)
      case Right(Right(a)) => Execution.from(a)
    })

  implicit def ExecutionEqual: Equal[Execution[Int]] =
    Equal.equal[Execution[Int]]((a, b) => {
      val ex1 = execute(a)
      val ex2 = execute(b)

      /*
       * Can't match the stack traces since for some of the exceptions the last part of the stack
       * trace is in the thread. While for others it is the main thread.
       */
      (ex1, ex2) match {
        case (Success(x), Success(y)) => x must_== y
        case (Failure(x), Failure(y)) => (x, y) match {
          case (r1: ResultException, r2: ResultException) => {
            r1.msg        must_== r2.msg
            r1.stacktrace must_== r2.stacktrace
            (r1.exception, r2.exception) match {
              case (Some(ex1), Some(ex2)) => ex1.getMessage must_== ex2.getMessage
              case (None, None)           => true
              case _                      => false
            }
          }
          case (r: ResultException, t: Throwable)         => r.getCause.getMessage must_== t.getMessage
          case (t: Throwable, r: ResultException)         => r.getCause.getMessage must_== t.getMessage
          case (t1: Throwable, t2: Throwable)             => t1.getMessage == t2.getMessage
        }
        case _                        => false
      }
    })
}
