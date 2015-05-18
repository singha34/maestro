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

package au.com.cba.omnia.maestro.core.upload

import scala.util.Random

import java.io.File
import java.io.FileNotFoundException

import org.specs2.execute.AsResult
import org.specs2.specification.Fixture

import org.apache.hadoop.fs.Path

/** Isolated directories for upload tests */
case class IsolatedDirs(rootDir: File) {
  val testDir         = new File(rootDir, "testInputDirectory")
  val testDirS        = testDir.toString

  val archiveDir      = new File(rootDir, "testArchiveDirectory")
  val archiveDirS     = archiveDir.toString

  val hdfsArchiveDir  = new File(rootDir.toString, "testHdfsArchiveDirectory")
  val hdfsArchiveDirS = hdfsArchiveDir.toString
  val hdfsArchiveDirP = new Path(hdfsArchiveDirS)

  val hdfsDir         = new File(rootDir, "testHdfsDirectory")
  val hdfsDirS        = hdfsDir.toString
}

/** Provides clean test directories for Upload tests */
object isolatedTest extends Fixture[IsolatedDirs] {
  val random = new Random() // Random is thread-safe
  val tmpDir = System.getProperty("java.io.tmpdir")

  def findUniqueRoot(): File = {
    val possibleDir = new File(tmpDir, "isolated-test-" + random.nextInt(Int.MaxValue)).getAbsoluteFile
    val unique = possibleDir.mkdir
    if (unique) possibleDir else findUniqueRoot() // don't expect to recurse often
  }

  // delete directory plus contents
  // WARNING: deleteAll will follow symlinks!
  // Java 7 supports symlinks, so once we drop Java 6 support we should be
  // able to do this properly, or use a library function that does it properly
  def deleteAll(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles.foreach(deleteAll)
    if (file.exists) {
      file.delete
      ()
    }
  }

  def apply[R: AsResult](test: IsolatedDirs => R) = {
    val dirs = IsolatedDirs(findUniqueRoot())

    List(dirs.testDir, dirs.archiveDir, dirs.hdfsDir) foreach (dir =>
      if (!dir.mkdir) throw new FileNotFoundException(dir.toString)
    )
    val result = AsResult(test(dirs))
    deleteAll(dirs.rootDir)
    result
  }
}
