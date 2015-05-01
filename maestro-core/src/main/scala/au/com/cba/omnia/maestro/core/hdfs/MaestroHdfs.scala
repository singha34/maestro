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

import org.apache.hadoop.fs.{FileSystem, Path}

import scalaz._, Scalaz._

import au.com.cba.omnia.permafrost.hdfs.Hdfs

import Guard.{NotProcessed, IngestionComplete}

/**
  * Utility functions for the [[https://commbank.github.io/permafrost/latest/api/index.html#au.com.cba.omnia.permafrost.hdfs.Hdfs Hdfs monad]],
  * with a Maestro flavour.
  *
  * This object was introduced in Maestro 2.10 to replace the [[Guard]] object, which served a similar purpose but
  * predated the Hdfs monad. [[Guard]] is now deprecated and ''will be removed'' in a future version. Users of
  * Maestro should update any code which uses [[Guard]] to use [[MaestroHdfs]] instead.
  *
  * ==Migration guide from `Guard` to `MaestroHdfs`==
  *
  * [[MaestroHdfs]] provides all the same functions as [[Guard]], with the same parameters, but instead of performing
  * the work immediately it returns an
  * [[https://commbank.github.io/permafrost/latest/api/index.html#au.com.cba.omnia.permafrost.hdfs.Hdfs Hdfs]] action.
  * This can be composed with other Hdfs actions into a more complex workflow. Alternatively, it can be composed with
  * other [[http://twitter.github.io/scalding/index.html#com.twitter.scalding.Execution Execution]] actions, with the
  * help of `Execution.fromHdfs`.
  *
  * Let's examine at a fictional project (loading data about widget sales) which uses some of the deprecated functions.
  *
  * {{{
  * // old code
  * case class WidgetSalesConfig[T <: ThriftStruct : Decode : Tag : Manifest](...)
  * {
  *   ...
  *   val inputs = Guard.expandTransferredPaths(s"${maestro.hdfsRoot}/source/${maestro.source}/${maestro.tablename}/*/*/*")
  * }}}
  *
  * Notice that `Guard.expandTransferredPaths` executes immediately when the class is instantiated (returning a list of
  * strings). This is a hidden side-effect. The benefit of the `Hdfs` monad is that it allows us to reason more easily
  * about the order in which operations are performed.
  *
  * If we change `Guard` to `MaestroHdfs`, then it will no longer compile because the return type has changed (from
  * `List[String]` to `Hdfs[List[String]]`). The simplest solution in this case is to move the call to `expandTransferredPaths`
  * later in the program, and keep only the glob here:
  *
  * {{{
  * // new code
  * case class WidgetSalesConfig[T <: ThriftStruct : Decode : Tag : Manifest](...)
  * {
  *   ...
  *   val inputGlob = s"${maestro.hdfsRoot}/source/${maestro.source}/${maestro.tablename}/*/*/*"
  * }}}
  *
  * Later in the file, we see where `inputs` was being used:
  *
  * {{{
  * // old code
  * def run[T <: ThriftStruct : Decode : Tag : Manifest](
  *   tableName      : String,
  *   partitionField : Field[T, String],
  *   loadWithKey    : Boolean = false
  * ): Execution[JobStatus] = for {
  *     conf             <- Execution.getConfig.map(WidgetSalesConfig(_, tableName, partitionField, loadWithKey))
  *     (pipe, loadInfo) <- load[T](conf.load, conf.inputs)                                                                    // here
  *     _                <- viewHive(conf.hiveTable, pipe)
  *     _                <- Execution.from(Guard.createFlagFile(conf.inputs))                                                  // and here
  *     _                <- Execution.fromHdfs { Hdfs.write(Hdfs.path(conf.processingPathFile), conf.inputs.mkString("\n")) }  // and here too
  *   } yield JobFinished
  * }}}
  *
  * This `for` block represents a sequence of actions in the Execution monad. The call to `expandTransferredPaths` can
  * be inserted here by wrapping it with `Execution.fromHdfs`. The same is required in order to replace `Guard.createFlagFile`
  * with `MaestroHdfs.createFlagFile`.
  *
  * {{{
  * // new code
  * def run[T <: ThriftStruct : Decode : Tag : Manifest](
  *   tableName      : String,
  *   partitionField : Field[T, String],
  *   loadWithKey    : Boolean = false
  * ): Execution[JobStatus] = for {
  *     conf             <- Execution.getConfig.map(WidgetSalesConfig(_, tableName, partitionField, loadWithKey))
  *     inputs           <- Execution.fromHdfs { MaestroHdfs.expandTransferredPaths(conf.findInputs) }                         // new line
  *     (pipe, loadInfo) <- load[T](conf.load, inputs)                                                                         // edited
  *     _                <- viewHive(conf.hiveTable, pipe)
  *     _                <- Execution.fromHdfs { MaestroHdfs.createFlagFile(inputs) }                                          // edited
  *     _                <- Execution.fromHdfs { Hdfs.write(Hdfs.path(conf.processingPathFile), inputs.mkString("\n")) }       // edited
  *   } yield JobFinished
  * }}}
  *
  * That's it! We have upgraded our dependency on the deprecated functions.
  */
object MaestroHdfs {
  /**
    * Expands the globs in the provided path and only keeps those directories that pass the filter.
    */
  def expandPaths(path: String, filter: GuardFilter = NotProcessed): Hdfs[List[String]] = for {
    paths   <- Hdfs.glob(Hdfs.path(path))  // already sorted (since permafrost 0.8.1)
    dirs    <- paths.filterM(Hdfs.isDirectory)
    passed  <- dirs.filterM(dir => Hdfs.withFilesystem(fs => filter.filter(fs, dir)))
  } yield passed.map(_.toString)

  /** Expand the complete file paths from the expandPaths, filtering out directories and 0 byte files */
  def listNonEmptyFiles(paths: List[String]): Hdfs[List[String]] = {
    val isNonEmpty = (filePath: Path) => Hdfs.withFilesystem(_.getFileStatus(filePath).getLen > 0)
    for {
      children <- paths.map(Hdfs.path).map(Hdfs.files(_)).sequence
      files    <- children.flatten.filterM(Hdfs.isFile)
      nonEmpty <- files.filterM(isNonEmpty)
    } yield nonEmpty.map(_.toString)
  }

  /** As `expandPath` but the filter is `NotProcessed` and `IngestionComplete`. */
  def expandTransferredPaths(path: String): Hdfs[List[String]] =
    expandPaths(path, NotProcessed &&& IngestionComplete)

  /** Creates the _PROCESSED flag to indicate completion of processing in given list of paths */
  def createFlagFile(directoryPaths: List[String]): Hdfs[Unit] = {
    directoryPaths
      .map(new Path(_, "_PROCESSED"))
      .map(Hdfs.create(_))
      .sequence_
  }
}
