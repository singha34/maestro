package au.com.cba.omnia.maestro.core.hdfs

import org.apache.hadoop.conf.Configuration

import org.specs2.execute.AsResult
import org.specs2.matcher.FileMatchers
import org.specs2.specification.AroundEach

import au.com.cba.omnia.thermometer.core.{ThermometerSpec, Thermometer}, Thermometer._

import au.com.cba.omnia.permafrost.test.HdfsMatchers

class MaestroHdfsSpec extends ThermometerSpec
  with AroundEach
  with FileMatchers
  with HdfsMatchers { def is = s2"""

MaestroHdfs properties
======================

  expandPaths:
    matches globbed dirs               $matchGlobbedDirs
    skips files                        $skipFiles
    skips processed dirs               $skipProcessed

  expandTransferredPaths:
    skips uningested dirs              $skipUningested

  listNonEmptyFiles:
    lists non-empty files              $listNonEmptyFiles
    skips subdirectories               $skipSubdirs

  createFlagFile:
    creates _PROCESSED                 $createPROCESSED
"""

  // Used by HdfsMatchers
  val conf = new Configuration

  // Every test executes with a copy of src/test/resources/hdfs-guard/
  def around[T: AsResult](t: => T) = withEnvironment(path(getClass.getResource("/hdfs-guard").toString)) { AsResult(t) }

  def matchGlobbedDirs = {
    MaestroHdfs.expandPaths(s"$dir/user/a*") must beValueLike {
      _ must containTheSameElementsAs(List(
        s"file:$dir/user/a",
        s"file:$dir/user/a1"
        // excludes various other directories that don't match the glob
      ))
    }
  }

  def skipFiles = {
    MaestroHdfs.expandPaths(s"$dir/user/b*") must beValueLike {
      _ must containTheSameElementsAs(List(
        s"file:$dir/user/b1"
        // excludes "b2" because it's not a directory
      ))
    }
  }

  def skipProcessed = {
    MaestroHdfs.expandPaths(s"$dir/user/c*") must beValueLike {
      _ must containTheSameElementsAs(List(
        s"file:$dir/user/c",
        s"file:$dir/user/c_transferred"
        // excludes "c_processed"
      ))
    }
  }

  def skipUningested = {
    MaestroHdfs.expandTransferredPaths(s"$dir/user/c*") must beValueLike {
      _ must containTheSameElementsAs(List(
        s"file:$dir/user/c_transferred"
        // excludes "c"
        // excludes "c_processed"
      ))
    }
  }

  def listNonEmptyFiles = {
    val paths = List(s"$dir/user/a", s"$dir/user/c")
    MaestroHdfs.listNonEmptyFiles(paths) must beValueLike {
      _ must containTheSameElementsAs(List(
        s"file:$dir/user/c/c.dat"
        // excludes "a/a.dat" because it is zero-length
      ))
    }
  }

  def skipSubdirs = {
    val paths = List(s"$dir/user")
    MaestroHdfs.listNonEmptyFiles(paths) must beValueLike {
      _ must containTheSameElementsAs(List(
        s"file:$dir/user/b2"
        // excludes all subdirectories
      ))
    }
  }

  def createPROCESSED = {
    val paths = List(s"$dir/user/a1", s"$dir/user/b1")
    MaestroHdfs.createFlagFile(paths) must beValue(())
    s"$dir/user/a1/_PROCESSED" must beAFilePath
    s"$dir/user/b1/_PROCESSED" must beAFilePath
  }
}
