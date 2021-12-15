lazy val commonSettings = Seq(
  organization := "works.worace",
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.12.12",
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "0.7.29" % Test
  ),
  testFrameworks += new TestFramework("munit.Framework")
)

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val core = project
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.0",
        "org.typelevel" %% "munit-cats-effect-3" % "1.0.0" % "test"
    )
  )
