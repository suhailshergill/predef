import sbt._
import sbt.Keys._

object Dependencies {
  object Versions {
    lazy val scala211 = "2.11.7"
    lazy val scala210 = "2.10.5"
    lazy val cats = "0.4.1"
    lazy val scalatest = "2.2.6"
    lazy val scalacheck = "1.12.5"
    lazy val shapelessContrib = "0.4"
    lazy val spark = "1.6.1"

    implicit class Ops(s: String) {
      def asMM: String = s.split('.').toSeq.init.mkString(".")
    }
  }
  import Versions.Ops

  lazy val `org.scala-lang:scala-reflect` =
    "org.scala-lang" % "scala-reflect" % Versions.scala211
  lazy val `com.chuusai:shapeless` = "com.chuusai" %% "shapeless" % "2.2.4"
  lazy val `org.typelevel:shapeless-spire` =
    "org.typelevel" %% "shapeless-spire" % Versions.shapelessContrib
  lazy val `org.typelevel:cats` = "org.typelevel" %% "cats" % "0.4.1"
  lazy val `org.specs2:eff-cats` = "org.specs2" %% "eff-cats" % "1.1"
  lazy val `com.github.mpilquist:simulacrum` =
    "com.github.mpilquist" %% "simulacrum" % "0.7.0"
  lazy val `org.typelevel:discipline` = "org.typelevel" %% "discipline" % "0.4"


  lazy val `org.spire-math:spire` = "org.spire-math" %% "spire" % "0.11.0"
  lazy val `org.apache.spark:spark-core` =
    "org.apache.spark" %% "spark-core" % Versions.spark excludeSparkDependencies()
  lazy val `org.apache.spark:spark-streaming` =
    "org.apache.spark" %% "spark-streaming" % Versions.spark excludeSparkDependencies()
  lazy val `org.apache.spark:spark-mllib` =
    "org.apache.spark" %% "spark-mllib" % Versions.spark excludeSparkDependencies()
  lazy val `org.apache.spark:spark-sql` =
    "org.apache.spark" %% "spark-sql" % Versions.spark excludeSparkDependencies()

  implicit class RichModuleId(m: ModuleID) {

    def excludeSparkDependencies(): ModuleID =
      m.
        exclude("commons-beanutils", "commons-beanutils-core").
        exclude("commons-collections", "commons-collections").
        exclude("commons-logging", "commons-logging").
        exclude("org.slf4j", "slf4j-log4j12").
        exclude("org.hamcrest", "hamcrest-core").
        exclude("junit", "junit").
        exclude("org.jboss.netty", "netty").
        exclude("com.esotericsoftware.minlog", "minlog")
  }

  lazy val `org.scalatest:scalatest` = "org.scalatest" %% "scalatest" % Versions.scalatest % "it,test"
  lazy val `org.scalactic:scalactic` = "org.scalactic" %% "scalactic" % Versions.scalatest
  lazy val `org.scalacheck:scalacheck` = "org.scalacheck" %% "scalacheck" % Versions.scalacheck % "test,it"
  lazy val `com.github.alexarchambault:scalacheck-shapeless` =
    "com.github.alexarchambault" %%
      ("scalacheck-shapeless_" + Versions.scalacheck.asMM) %
      "0.3.1"

  /*
   * Dependency groups.
   */

  lazy val scalatest: Def.Setting[_] = libraryDependencies ++= Seq(
    `org.scalatest:scalatest`
    , `org.scalactic:scalactic`
    , `org.scalacheck:scalacheck`
    // , `com.github.alexarchambault:scalacheck-shapeless`
  )

  lazy val base: Def.Setting[_] = libraryDependencies ++= Seq(
    `org.scala-lang:scala-reflect`
    , `com.chuusai:shapeless`
    // `org.typelevel:shapeless-spire` conflicts with recent version of spire
    // , `org.typelevel:shapeless-spire`
    , `org.typelevel:cats`
    // , `org.specs2:eff-cats`
    , `com.github.mpilquist:simulacrum`
    , `org.typelevel:discipline`
  )

  lazy val numbers: Def.Setting[_] = libraryDependencies ++= Seq(
    `org.spire-math:spire`
    // , `org.apache.spark:spark-core`
    // , `org.apache.spark:spark-streaming`
    // , `org.apache.spark:spark-mllib`
    // , `org.apache.spark:spark-sql`
  )

  lazy val overrides: Seq[Setting[_]] = dependencyOverrides ++= Set(
    "org.scala-lang" % "scala-compiler" % Versions.scala211
    , "org.scala-lang" % "scala-library" % Versions.scala211
  )

}
