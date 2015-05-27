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

import sbt._
import Keys._


import com.twitter.scrooge.ScroogeSBT._

import sbtassembly.AssemblyPlugin.autoImport.assembly

import sbtunidoc.Plugin.{ScalaUnidoc, UnidocKeys}
import UnidocKeys.{unidoc, unidocProjectFilter}

import au.com.cba.omnia.uniform.core.standard.StandardProjectPlugin._
import au.com.cba.omnia.uniform.core.version.UniqueVersionPlugin._
import au.com.cba.omnia.uniform.dependency.UniformDependencyPlugin._
import au.com.cba.omnia.uniform.thrift.UniformThriftPlugin._
import au.com.cba.omnia.uniform.assembly.UniformAssemblyPlugin._

import au.com.cba.omnia.humbug.HumbugSBT._

object build extends Build {
  type Sett = Def.Setting[_]

  val thermometerVersion = "1.0.0-20150513002558-a6bcf7f"
  val ebenezerVersion    = "0.18.0-20150518023229-cda03fa"
  val omnitoolVersion    = "1.10.0-20150430044321-3ca9118"
  val permafrostVersion  = "0.8.1-20150507233550-af2c035"
  val edgeVersion        = "3.4.0-20150513004502-4b6d30d"
  val humbugVersion      = "0.6.1-20150513010955-5eb6297"
  val parlourVersion     = "1.9.0-20150514050504-a530fa1"

  val scalikejdbc = noHadoop("org.scalikejdbc" %% "scalikejdbc" % "2.2.6")
    .exclude("org.joda", "joda-convert")
    .exclude("org.scala-lang.modules", "scala-parser-combinators_2.11")

  lazy val standardSettings: Seq[Sett] =
    Defaults.coreDefaultSettings ++
    uniformDependencySettings ++
    strictDependencySettings ++
    uniform.docSettings("https://github.com/CommBank/maestro") ++
    Seq(
      logLevel in assembly := Level.Error,
      // Run tests sequentially across the subprojects.
      concurrentRestrictions in Global := Seq(
        Tags.limit(Tags.Test, 1)
      )
    )

  lazy val all = Project(
    id = "all"
  , base = file(".")
  , settings =
       standardSettings
    ++ uniform.project("maestro-all", "au.com.cba.omnia.maestro")
    ++ uniform.ghsettings
    ++ Seq[Sett](
         publishArtifact := false
       , addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
       , unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(example, schema, benchmark)
    )
  , aggregate = Seq(core, macros, api, test, schema)
  )

  lazy val api = Project(
    id = "api"
  , base = file("maestro-api")
  , settings =
       standardSettings
    ++ uniform.project("maestro", "au.com.cba.omnia.maestro.api")
    ++ Seq[Sett](
      libraryDependencies ++= depend.hadoopClasspath ++ depend.hadoop() ++ depend.testing()
    )
  ).dependsOn(core)
   .dependsOn(macros)

  lazy val core = Project(
    id = "core"
  , base = file("maestro-core")
  , settings =
       standardSettings
    ++ uniformThriftSettings
    ++ uniform.project("maestro-core", "au.com.cba.omnia.maestro.core")
    ++ humbugSettings
    ++ Seq[Sett](
      scroogeThriftSourceFolder in Test <<= (sourceDirectory) { _ / "test" / "thrift" / "scrooge" },
      humbugThriftSourceFolder in Test <<= (sourceDirectory) { _ / "test" / "thrift" / "humbug" },
      libraryDependencies ++=
           depend.scalaz() 
        ++ depend.scalding()
        ++ depend.hadoopClasspath
        ++ depend.hadoop()
        ++ depend.shapeless() ++ depend.testing() ++ depend.time()
        ++ depend.parquet()
        ++ depend.omnia("ebenezer-hive", ebenezerVersion)
        ++ depend.omnia("permafrost",    permafrostVersion)
        ++ depend.omnia("edge",          edgeVersion)
        ++ depend.omnia("humbug-core",   humbugVersion)
        ++ depend.omnia("omnitool-time", omnitoolVersion)
        ++ depend.omnia("omnitool-file", omnitoolVersion)
        ++ depend.omnia("parlour",       parlourVersion)
        ++ Seq(
          noHadoop("commons-validator"  % "commons-validator" % "1.4.0"),
          "org.specs2"                 %% "specs2-matcher-extra" % "3.5" % "test"
            exclude("org.scala-lang", "scala-compiler"),
          "au.com.cba.omnia"           %% "ebenezer-test"     % ebenezerVersion        % "test",
          "au.com.cba.omnia"           %% "thermometer-hive"  % thermometerVersion     % "test",
          scalikejdbc                                                                  % "test",
          "com.opencsv"                 % "opencsv"           % "3.3"
            exclude ("org.apache.commons", "commons-lang3") // conflicts with hive
        ),
      parallelExecution in Test := false
    )
  )

  lazy val macros = Project(
    id = "macros"
  , base = file("maestro-macros")
  , settings =
       standardSettings
    ++ uniform.project("maestro-macros", "au.com.cba.omnia.maestro.macros")
    ++ Seq[Sett](
         libraryDependencies <++= scalaVersion.apply(sv => Seq(
          "org.scala-lang"   % "scala-compiler" % sv
            exclude("org.scala-lang.modules", "scala-parser-combinators_2.11")
            exclude("org.scala-lang.modules", "scala-xml_2.11")
         , "org.scala-lang"   % "scala-reflect"  % sv
         ) ++ depend.testing())
       , addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
    )
  ).dependsOn(core)
   .dependsOn(test % "test")

  lazy val schema = Project(
    id = "schema"
  , base = file("maestro-schema")
  , settings =
       standardSettings
    ++ uniform.project("maestro-schema", "au.com.cba.omnia.maestro.schema")
    ++ uniformAssemblySettings
    ++ Seq[Sett](
          libraryDependencies <++= scalaVersion.apply(sv => Seq(
            "com.quantifind"     %% "sumac"         % "0.3.0"
          , "org.scala-lang"     %  "scala-reflect" % sv
          , "org.apache.commons" %  "commons-lang3" % "3.1"
          ) ++ depend.scalding() ++ depend.hadoopClasspath ++ depend.hadoop())
       )
    )

  lazy val example = Project(
    id = "example"
  , base = file("maestro-example")
  , settings =
       standardSettings
    ++ uniform.project("maestro-example", "au.com.cba.omnia.maestro.example")
    ++ uniformAssemblySettings
    ++ uniformThriftSettings
    ++ Seq[Sett](
         libraryDependencies ++= depend.hadoopClasspath ++ depend.hadoop() ++ depend.parquet() ++ Seq(
           scalikejdbc % "test"
         )
       , parallelExecution in Test := false
       , sources in doc in Compile := List() 
       , addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
    )
  ).dependsOn(core)
   .dependsOn(macros)
   .dependsOn(api)
   .dependsOn(test % "test")

  lazy val benchmark = Project(
    id = "benchmark"
  , base = file("maestro-benchmark")
  , settings =
       standardSettings
    ++ uniform.project("maestro-benchmark", "au.com.cba.omnia.maestro.benchmark")
    ++ uniformThriftSettings
    ++ Seq[Sett](
      libraryDependencies ++= Seq(
        "com.storm-enroute" %% "scalameter" % "0.6"
          exclude("org.scala-lang.modules", "scala-parser-combinators_2.11")
          exclude("org.scala-lang.modules", "scala-xml_2.11")
      ) ++ depend.testing()
    , testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
    , parallelExecution in Test := false
    , logBuffered := false
    )
  ).dependsOn(core)
   .dependsOn(macros)
   .dependsOn(api)

  lazy val test = Project(
    id = "test"
  , base = file("maestro-test")
  , settings =
       standardSettings
    ++ uniform.project("maestro-test", "au.com.cba.omnia.maestro.test")
    ++ uniformThriftSettings
    ++ humbugSettings
    ++ Seq[Sett](
         scroogeThriftSourceFolder in Compile <<= (sourceDirectory) { _ / "main" / "thrift" / "scrooge" }
       , humbugThriftSourceFolder  in Compile <<= (sourceDirectory) { _ / "main" / "thrift" / "humbug" }
       , libraryDependencies ++=
           depend.omnia("ebenezer-test",    ebenezerVersion)
           ++ depend.omnia("thermometer-hive", thermometerVersion)
           ++ depend.hadoopClasspath ++ depend.hadoop()
           ++ depend.testing(configuration = "test")
    )
  ).dependsOn(core)
}
