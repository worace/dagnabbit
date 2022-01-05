import xerial.sbt.Sonatype._

lazy val commonSettings = Seq(
  organization := "works.worace",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.12.12",
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "0.7.29" % Test
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  homepage := Some(url("https://github.com/worace/dagnabbit")),
  licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  sonatypeProjectHosting := Some(GitHubHosting("worace", "dagnabbit", "horace@worace.works")),
  publishTo := sonatypePublishToBundle.value,
  ThisBuild / versionScheme := Some("early-semver"),
  developers := List(
    Developer(
      "worace",
      "Horace Williams",
      "horace@worace.works",
      url("https://worace.works")
    )
  ),
  publishArtifact := false
)

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val core = project
  .settings(commonSettings: _*)
  .settings(
    name := "dagnabbit-core",
    publishArtifact := true,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.0",
        "org.typelevel" %% "munit-cats-effect-3" % "1.0.0" % "test"
    )
  )
