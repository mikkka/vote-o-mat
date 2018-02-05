val catsVersion = "1.0.0"
val catsAll = Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-macros" % catsVersion,
  "org.typelevel" %% "cats-kernel" % catsVersion
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

val _organization = "name.mtkachev"
val _scalaVersion = "2.12.4"

scalaVersion in ThisBuild := _scalaVersion

lazy val root = (project in file(".")).
  settings(
    organization := _organization,
    name := "vote-o-mat",
    scalaVersion := _scalaVersion,
    libraryDependencies ++= catsAll,
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:_"
    )
  )