name := "List 09"

scalaVersion := "2.13.4"

coverageEnabled := true
coverageMinimum := 80
coverageFailOnMinimum := true
//SIP Nice that this coverage-check is added

import scalariform.formatter.preferences._

scalariformPreferences := scalariformPreferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentConstructorArguments, true)
  .setPreference(DanglingCloseParenthesis, Preserve)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "utf-8",
    "-explaintypes",
    "-language:experimental.macros",
    "-language:implicitConversions",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yrangepos",
  )
)

lazy val commonDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.1.2" % "test"
)

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .aggregate(
    blackjack,
    core
  )
  .dependsOn(core, blackjack)

lazy val blackjack = project
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(core)

lazy val core = project
  .settings(
    commonSettings,
    libraryDependencies ++= commonDependencies
  )
