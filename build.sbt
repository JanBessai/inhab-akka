lazy val commonSettings = Seq(
  version := "0.0.1",
  organization := "de.tu_dortmund.cs.ls14",
  
  scalaVersion := "2.12.2",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.typesafeRepo("releases")
  ),

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  )
)

lazy val root = (Project(id = "inhab-akka", base = file(".")))
  .settings(commonSettings: _*)
  .settings(
    moduleName := "inhab-akka",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-typed" % "2.5.3",
      "com.typesafe.akka" %% "akka-testkit" % "2.5.3" % Test,
      "org.scalatest" %% "scalatest" % "3.0.1" % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
    )
  )

