import Dependencies.Versions
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging

lazy val Name = "predef"

organization in ThisBuild := "sss"
organizationName in ThisBuild := "Suhail Shergill"
name in ThisBuild         := Name


lazy val defaultSettings = Defaults.itSettings ++
  sbtCompilerPlugins.settings ++
  sbtScalariform.settings ++
  sbtWartremover.settings ++
  scalacOptions.settings ++
  Dependencies.overrides ++
  Seq(
    CommonResolvers.settings,
    Dependencies.base, Dependencies.scalatest, Dependencies.numbers,
    retrieveManaged := true,
    publishMavenStyle := true,
    conflictManager in ThisBuild := ConflictManager.latestCompatible,
    Version.set("0.1", Version.SNAPSHOT),
    scalaVersion in ThisBuild := Versions.scala211,
    // unclear how to combine below with 'Dependencies.overrides'
    // crossScalaVersions in ThisBuild := Seq(Versions.scala210, Versions.scala211),
    fork in Test := true,
    // Show current project name in the SBT prompt, e.g. `predef>`
    shellPrompt in ThisBuild := { state =>
      Project.extract(state).currentRef.project + "> " }
  )


lazy val root = Project(Name, file(".")).
  configs(IntegrationTest).
  settings(defaultSettings: _*).
  enablePlugins(JavaAppPackaging).
  settings(testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test")))).
  settings(testOptions in IntegrationTest := Seq(Tests.Filter(s => s.endsWith("Test")))).
  settings(parallelExecution in IntegrationTest := false)
