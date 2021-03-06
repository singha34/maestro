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

package au.com.cba.omnia.maestro.core.hive

import scalaz._, Scalaz._

import org.apache.hadoop.fs.Path

import org.apache.hadoop.hive.conf.HiveConf
import org.apache.hadoop.hive.conf.HiveConf.ConfVars.METASTOREWAREHOUSE

import com.twitter.scalding.{Hdfs => _, _}

import com.twitter.scrooge.ThriftStruct

import au.com.cba.omnia.permafrost.hdfs.Hdfs

import au.com.cba.omnia.ebenezer.scrooge.ParquetScroogeSource
import au.com.cba.omnia.ebenezer.scrooge.hive._

import au.com.cba.omnia.maestro.core.partition.Partition
import au.com.cba.omnia.maestro.core.scalding.ConfHelper
import au.com.cba.omnia.maestro.core.scalding.ExecutionOps._

/** 
  * Base type for partitioned and unpartitioned Hive tables. 
  * ST - Type parameter of [[TypedSink]] and [[TypedPipe]]
  * returned in `sink` and `write` methods respectively. 
  */
sealed trait HiveTable[T <: ThriftStruct, ST] {
  /** Fully qualified SQL reference to table.*/
  val name: String = s"$database.$table"

  /** Path of the table. */
  lazy val path = new Path(externalPath.getOrElse(
    s"${(new HiveConf()).getVar(METASTOREWAREHOUSE)}/$database.db/$table"
  ))

  def database: String
  def table: String
  def externalPath: Option[String]

  /** Creates a scalding source to read from the hive table.*/
  def source: Mappable[T]
  /** Creates [[Execution]] that writes to `this.sink` using given [[TypedPipe]].*/
  def writeExecution(pipe: TypedPipe[T], append: Boolean = true): Execution[ExecutionCounters]
}

/** Information need to address/describe a specific partitioned hive table.*/
case class PartitionedHiveTable[A <: ThriftStruct : Manifest, B : Manifest : TupleSetter](
  database: String, table: String, partition: Partition[A, B], externalPath: Option[String] = None
) extends HiveTable[A, (B, A)] {

  /** List of partition column names and type (string by default). */ 
  val partitionMetadata: List[(String, String)] = partition.fieldNames.map(n => (n, "string"))

  override def source: PartitionHiveParquetScroogeSource[A] =
    PartitionHiveParquetScroogeSource[A](database, table, partitionMetadata)

  override def writeExecution(pipe: TypedPipe[A], append: Boolean = true): Execution[ExecutionCounters] = {
    def modifyConfig(config: Config) =
      if (append) ConfHelper.createUniqueFilenames(config)
      else        config

    val execution = 
      pipe
        .map(v => partition.extract(v) -> v)
        .writeExecution(PartitionHiveParquetScroogeSink[B, A](database, table, partitionMetadata, externalPath, append))
        .getAndResetCounters
        .map(_._2)

    execution.withSubConfig(modifyConfig)
  }
}

/** Information need to address/describe a specific unpartitioned hive table.*/
case class UnpartitionedHiveTable[A <: ThriftStruct : Manifest](
  database: String, table: String, externalPath: Option[String] = None
) extends HiveTable[A, A] {

  override def source =
    HiveParquetScroogeSource[A](database, table, externalPath)

  /** Writes the contents of the pipe to this HiveTable. */
  override def writeExecution(pipe: TypedPipe[A], append: Boolean = true): Execution[ExecutionCounters] = {
    // Creates the hive table and gets its path
    val setup = Execution.fromHive(
      Hive.createParquetTable[A](database, table, List.empty, externalPath.map(new Path(_))) >>
      Hive.getPath(database, table)
    )

    // Runs the scalding job and gets the counters
    def write(path: Path) = pipe
      .writeExecution(ParquetScroogeSource[A](path.toString))
      .getAndResetCounters
      .map(_._2)

    //If we are appending the files are written to a temporary location and then copied accross
    if (append) {
      def moveFiles(src: Path, dst: Path): Hdfs[Unit] = for {
        files <- Hdfs.files(src, "*.parquet")
        time  =  System.currentTimeMillis
        _     <- Hdfs.mkdirs(dst)
        _     <- files.zipWithIndex.traverse {
                   case (file, idx) => Hdfs.move(file, new Path(dst, f"part-$time-$idx%05d.parquet"))
                 }
      } yield ()

      setup.flatMap(dst => Execution.fromHdfs(Hdfs.createTempDir()).bracket(
        tmpDir => Execution.fromHdfs(Hdfs.delete(tmpDir, true))
      )(
        tmpDir => for {
          counters <- write(tmpDir)
          _        <- Execution.fromHdfs(moveFiles(tmpDir, dst))
        } yield counters
      ))
    } else {
      for {
        dst      <- setup
        counters <- write(dst)
      } yield counters
    }
  }
}

/** Contructors for HiveTable. */
object HiveTable {
  /** Information need to address/describe a specific partitioned hive table.*/
  def apply[A <: ThriftStruct : Manifest, B : Manifest : TupleSetter](
    database: String, table: String, partition: Partition[A, B], path: String
  ): HiveTable[A, (B,A)] =
    PartitionedHiveTable(database, table, partition, Some(path))

  /** Information need to address/describe a specific partitioned hive table.*/
  def apply[A <: ThriftStruct : Manifest, B : Manifest : TupleSetter](
    database: String, table: String, partition: Partition[A, B], pathOpt: Option[String] = None
  ): HiveTable[A, (B, A)] =
    PartitionedHiveTable(database, table, partition, pathOpt)

  /** Information need to address/describe a specific unpartitioned hive table.*/
  def apply[A <: ThriftStruct : Manifest](
    database: String, table: String, path: String
  ): HiveTable[A, A] =
    new UnpartitionedHiveTable(database, table, Some(path))

  /** Information need to address/describe a specific unpartitioned hive table.*/
  def apply[A <: ThriftStruct : Manifest](
    database: String, table: String, pathOpt: Option[String] = None
  ): HiveTable[A, A] =
    UnpartitionedHiveTable(database, table, pathOpt)
}
