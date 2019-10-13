scalaVersion := "2.12.8"

name := "a-study-in-scala"

libraryDependencies := Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.typelevel" %% "cats-core" % "1.6.0" withSources() withJavadoc(),
  "org.typelevel" %% "cats-effect" % "1.3.0" withSources() withJavadoc()
)

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification",
  "-deprecation",
  "-feature",
  "-language:higherKinds"
)