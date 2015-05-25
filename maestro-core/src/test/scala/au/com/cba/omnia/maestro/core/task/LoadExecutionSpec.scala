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

import scalaz._, Scalaz._

import com.twitter.scalding.{Execution, Stat}
import com.twitter.scalding.typed.IterablePipe

import au.com.cba.omnia.thermometer.core.{ThermometerSpec, Thermometer}, Thermometer._

import au.com.cba.omnia.maestro.core.clean.Clean
import au.com.cba.omnia.maestro.core.filter.RowFilter
import au.com.cba.omnia.maestro.core.split.Splitter
import au.com.cba.omnia.maestro.core.time.TimeSource
import au.com.cba.omnia.maestro.core.validate.Validator
import au.com.cba.omnia.maestro.core.scalding.StatKeys

import au.com.cba.omnia.maestro.innercore.thrift.scrooge.{StringPair, StringTriple}

private object LoadExec extends LoadExecution

object LoadExecutionSpec extends ThermometerSpec with StringPairSupport with StringTripleSupport { def is = s2"""

Load execution properties
=========================

  can load using execution monad                                 $normal
  can load fixed length using execution monad                    $fixed
  returns the right load info for no data                        $noData
  returns the right load info for an acceptable number of errors $someErrors
  returns the right load info for too many errors                $manyErrors
  calculates the right number of rows after filtering            $filtered
  can load while generating keys                                 $normalGenKey
  load fails rows with too much data                             $tooLong
  load fails rows with not enough data                           $tooShort

"""

  val conf = LoadConfig[StringPair](errors = "errors", timeSource = TimeSource.now(), none = "null")

  def normal = {
    withEnvironment(path(getClass.getResource("/load-execution").toString)) {
      val exec = LoadExec.load[StringPair](conf, List("normal"))
      executesSuccessfully(exec)._2 must_== LoadSuccess(4, 4, 4, 0)
    }
  }

  def fixed = {
    withEnvironment(path(getClass.getResource("/load-execution").toString)) {
      val fixedConf = conf.copy(splitter = Splitter.fixed(List(4)))
      val exec      = LoadExec.load[StringPair](fixedConf, List("fixed"))
      executesSuccessfully(exec)._2 must_== LoadSuccess(4, 4, 4, 0)
    }
  }

  def noData = {
    withEnvironment(path(getClass.getResource("/load-execution").toString)) {
      val exec = LoadExec.load[StringPair](conf, List("no-data"))
      executesSuccessfully(exec)._2 must_== EmptyLoad
    }
  }

  def someErrors = {
    withEnvironment(path(getClass.getResource("/load-execution").toString)) {
      val tolerantConf = conf.copy(errorThreshold = 0.25)
      val exec         = LoadExec.load[StringPair](tolerantConf, List("some-errors"))
      executesSuccessfully(exec)._2 must_== LoadSuccess(5, 5, 4, 1)
    }
  }

  def manyErrors = {
    withEnvironment(path(getClass.getResource("/load-execution").toString)) {
      val tolerantConf = conf.copy(errorThreshold = 0.25)
      val exec         = LoadExec.load[StringPair](tolerantConf, List("many-errors"))
      executesSuccessfully(exec)._2 must_== LoadFailure(5, 5, 3, 2)
    }
  }

  def filtered = {
    withEnvironment(path(getClass.getResource("/load-execution").toString)) {
      val filteredConf = conf.copy(errorThreshold = 0.25, filter = RowFilter.byRowLeader("D"))
      val exec         = LoadExec.load[StringPair](filteredConf, List("filtered"))
      executesSuccessfully(exec)._2 must_== LoadSuccess(5, 3, 3, 0)
    }
  }

  def normalGenKey = {
    val confGenKey = LoadConfig[StringTriple](
      errors = "errors", timeSource = TimeSource.now(), none = "null", generateKey = true
    )

    withEnvironment(path(getClass.getResource("/load-execution").toString)) {
      val exec             = LoadExec.load[StringTriple](confGenKey, List("normal"))
      val (pipe, loadInfo) = executesSuccessfully(exec)
      loadInfo must_== LoadSuccess(4, 4, 4, 0)
      // Check that the keys are unique.
      executesSuccessfully(pipe.distinctBy(_._3).map(_ => 1).sum.getExecution) must_== 4
    }
  }

  def tooLong = {
    val tripleConf = LoadConfig[StringTriple](
      errors = "errors", timeSource = TimeSource.now(), none = "null"
    )

    val data = List(
      RawRow("a|b", List("19850823")),
      RawRow("a|b|c", List("19850823"))
    )

    val x = Execution.withId { id =>
      val stat = Stat(StatKeys.tuplesFiltered)(id)
      LoadEx.parseRows[StringTriple](tripleConf, stat, IterablePipe(data)).toIterableExecution
    }

    executesSuccessfully(x) must_== List(
      StringTriple("a", "b", "19850823").right,
      "Too many cells in the row. Got 4 expected 3.".left
    )
  }

  def tooShort = {
    val tripleConf = LoadConfig[StringTriple](
      errors = "errors", timeSource = TimeSource.now(), none = "null"
    )

    val data = List(
      RawRow("a|b", List("19850823")),
      RawRow("a", List("19850823"))
    )

    val x = Execution.withId { id =>
      val stat = Stat(StatKeys.tuplesFiltered)(id)
      LoadEx.parseRows[StringTriple](tripleConf, stat, IterablePipe(data)).toIterableExecution
    }

    executesSuccessfully(x) must_== List(
      StringTriple("a", "b", "19850823").right,
      "Not enough cells in the row. Got 2 expected 3.".left
    )
  }
}
