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
package upload

import org.specs2.Specification
import org.specs2.matcher.ThrownExpectations

import java.io.File

import org.joda.time.DateTime

import scalaz.{\/, -\/, \/-}

import au.com.cba.omnia.omnitool.{Error, Ok}

class InputSpec extends Specification with ThrownExpectations { def is = s2"""

Input properties
================

control files
-------------

  .CTL is control file ext     $ctlIsControl
  .CTR is control file ext     $ctrIsControl
  S_ is control file prefix    $sIsControl
  CTR in middle is not control $ctrMiddleIsNotControl

 findFiles filter
 ----------------

  reject date starting late      $rejectLateDate
  reject literal starting late   $rejectLateLiteral
  reject missing src dir         $rejectMissingSourceDir
  label control files            $labelControlFiles
"""

 def ctlIsControl =
    Input.isControl(new File("local.CTL"), ControlPattern.default) must beTrue

  def ctrIsControl =
    Input.isControl(new File("local.CTR"), ControlPattern.default) must beTrue

  def sIsControl =
    Input.isControl(new File("S_LOCAL"), ControlPattern.default) must beTrue

  def ctrMiddleIsNotControl =
    Input.isControl(new File("ABC_DEF_CTRP_GHIJK_ABC209"), ControlPattern.default) must beFalse

  def rejectLateDate = isolatedTest((dirs: IsolatedDirs) => {
    val f1 = new File(dirs.testDirS, "local20140506.txt")
    val f2 = new File(dirs.testDirS, "localname20140506.txt")
    val data1 = DataFile(f1.toString, List("2014", "06", "05") mkString File.separator)
    f1.createNewFile
    f2.createNewFile

    val fileList = Input.findFiles(dirs.testDirS, "local", "{table}{yyyyddMM}.txt", ControlPattern.default)
    fileList mustEqual \/-(InputFiles(List(), List(data1)))
  })

  def rejectLateLiteral = isolatedTest((dirs: IsolatedDirs) => {
    val f1 = new File(dirs.testDirS, "yahoolocal20140506.txt")
    val f2 = new File(dirs.testDirS, "localname20140506.txt")
    val data2 = DataFile(f2.toString, List("2014", "06", "05") mkString File.separator)
    f1.createNewFile
    f2.createNewFile

    val fileList = Input.findFiles(dirs.testDirS, "local", "{table}*{yyyyddMM}.txt", ControlPattern.default)
    fileList mustEqual \/-(InputFiles(List(), List(data2)))
  })

  def rejectMissingSourceDir = isolatedTest((dirs: IsolatedDirs) => {
    dirs.testDir.delete
    val fileList = Input.findFiles(dirs.testDirS, "local", "yyyyMMdd", ControlPattern.default)
    fileList must beLike { case -\/(_) => ok }
  })

  def labelControlFiles = isolatedTest((dirs: IsolatedDirs) => {
    val f1 = new File(dirs.testDirS, "local20140605.CTL")
    val f2 = new File(dirs.testDirS, "local20140605.DAT")
    val ctrl1 = ControlFile(f1)
    val data2 = DataFile(f2.toString, List("2014", "06", "05") mkString File.separator)
    f1.createNewFile
    f2.createNewFile

    val fileList = Input.findFiles(dirs.testDirS, "local", "{table}{yyyyMMdd}*", ControlPattern.default)
    fileList must_== \/-(InputFiles(List(ctrl1), List(data2)))
  })
}
