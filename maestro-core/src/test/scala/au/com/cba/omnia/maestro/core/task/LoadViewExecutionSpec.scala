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

import java.io._

import scala.util.Random

import com.twitter.scalding.Execution
import com.twitter.scalding.typed.TypedPipe

import cascading.tap.hadoop.HfsProps

import au.com.cba.omnia.ebenezer.scrooge.hive.Hive
import au.com.cba.omnia.ebenezer.ParquetLogging

import au.com.cba.omnia.thermometer.core.{ThermometerSpec, Thermometer}, Thermometer._
import au.com.cba.omnia.thermometer.fact.Fact
import au.com.cba.omnia.thermometer.hive.HiveSupport

import au.com.cba.omnia.maestro.core.clean.Clean
import au.com.cba.omnia.maestro.core.filter.RowFilter
import au.com.cba.omnia.maestro.core.hive.HiveTable
import au.com.cba.omnia.maestro.core.partition.Partition
import au.com.cba.omnia.maestro.core.scalding.ExecutionOps._
import au.com.cba.omnia.maestro.core.split.Splitter
import au.com.cba.omnia.maestro.core.time.TimeSource
import au.com.cba.omnia.maestro.core.validate.Validator

import au.com.cba.omnia.maestro.innercore.thrift.scrooge.StringPair

object LoadViewExecutionSpec extends ThermometerSpec
    with StringPairSupport
    with HiveSupport
    with ParquetLogging { def is = s2"""

Load + View execution properties
================================

  load + view should merge splits      $viewMerges
  load + viewHive should merge splits  $viewHiveMerges

"""

  val date = "today"

  def loadConf = LoadConfig[StringPair](
    errors = "errors",
    timeSource = TimeSource.predetermined(date)
  )

  def viewMerges = {
    val inputFile = dir + "/view/input"
    val outputDir = dir + "/view/output"
    val viewConf  = ViewConfig[StringPair, String](
      partition   = Partition(List("SECOND"), _.second, "%s"),
      output      = outputDir
    )

    // the date in the second column is pre-determined, hence only one partition
    // with path $outputDir/$date
    testMerge(
      inputFile,
      s"$outputDir/$date",
      pipe => Exec.view[StringPair, String](viewConf, pipe)
    )
  }

  def viewHiveMerges = {
    val inputFile = dir + "/viewHive/input"
    val outputDir = dir + "/viewHive/output"
    val hiveTable = HiveTable[StringPair]("mydb", "mytbl", Some(outputDir))

    testMerge(
      inputFile,
      outputDir,
      pipe => Exec.viewHive(hiveTable, pipe)
    )
  }

  def testMerge(inputFile: String, outputDir: String, writeOutput: TypedPipe[StringPair] => Execution[_]) = {
    val kb = 1024

    val exec = for {
      _         <- Execution.from(writeInput(inputFile, 10*kb))
      (pipe, _) <- Exec.load(loadConf.copy(splitSize = 5*kb), List(inputFile))
      _         <- writeOutput(pipe)
    } yield ()

    jobConf.setInt("fs.local.block.size", kb)
    executesOk(exec)
    facts(
      // input gets blown up into larger temp sequence file, so not sure of the
      // exact number of blocks we are merging, but the blow up should not be
      // anywhere near a factor of 5, so we expect the number of final files to
      // be less than 10
      Fact(_.glob(outputDir </> "part-*").length must be_>(1) and be_<(10))
    )
  }

  // write input with 31 chars per line
  // with the newline, this gives 32 chars per line, and as we are using
  // alphanumeric chars with UTF8 format, we get 32 bytes per line
  def writeInput(path: String, length: Int) {
    val sep = '|'
    val file = new File(path)
    if (!file.getParentFile.mkdirs) { throw new Exception(s"couldn't create parent dirs to $path") }
    if (!file.createNewFile) { throw new Exception(s"couldn't create $path") }

    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8"))
    try {
      val perLine = 32
        (new Random).alphanumeric
        .filter(_ != sep)
        .grouped(perLine-1).map(_.mkString("", "", "\n"))
        .take(length / perLine)
        .foreach(writer.write(_))
    } finally {
      writer.close
    }
  }

  private object Exec extends LoadExecution with ViewExecution
}
