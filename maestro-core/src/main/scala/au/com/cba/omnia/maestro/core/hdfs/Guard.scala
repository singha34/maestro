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
package hdfs

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileSystem, Path}

import au.com.cba.omnia.permafrost.hdfs.Hdfs

case class GuardFilter(filter: (FileSystem, Path) => Boolean) {
  def &&&(that: GuardFilter): GuardFilter =
    GuardFilter((fs, p) => filter(fs, p) && that.filter(fs, p))
}

/**
  * (DEPRECATED) Utility functions that operate on the Hadoop filesystem
  *
  * ''Use [[MaestroHdfs]] instead (same method names, but return type is Hdfs).''
  */
object Guard {
  /** Filter out any directories that HAVE a _PROCESSED file. */
  val NotProcessed = GuardFilter((fs, p) => !fs.exists(new Path(p, "_PROCESSED")))
  /** Filter out any directories that DO NOT HAVE a _INGESTION_COMPLETE file. */
  val IngestionComplete = GuardFilter((fs, p) => fs.exists(new Path(p, "_INGESTION_COMPLETE")))

  /** Expands the globs in the provided path and only keeps those directories that pass the filter. */
  def expandPaths(path: String, filter: GuardFilter = NotProcessed): List[String] = runHdfs(MaestroHdfs.expandPaths(path, filter))

  /** Expand the complete file paths from the expandPaths, filtering out directories and 0 byte files */
  def listNonEmptyFiles(paths: List[String]): List[String] = runHdfs(MaestroHdfs.listNonEmptyFiles(paths))

  /** As `expandPath` but the filter is `NotProcessed` and `IngestionComplete`. */
  def expandTransferredPaths(path: String): List[String] = runHdfs(MaestroHdfs.expandTransferredPaths(path))

  /** Creates the _PROCESSED flag to indicate completion of processing in given list of paths */
  def createFlagFile(directoryPath : List[String]): Unit = runHdfs(MaestroHdfs.createFlagFile(directoryPath))

  lazy val conf = new Configuration

  def runHdfs[A](hdfs: Hdfs[A]): A =
    hdfs.run(conf).foldAll(
      a       => a,
      msg     => throw new Exception(msg),
      ex      => throw ex,
      (_, ex) => throw ex
    )
}
