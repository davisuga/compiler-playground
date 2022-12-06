scalaVersion := "3.2.1"

// enablePlugins(ScalaNativePlugin)
val crossLibs = Seq(
  "org.tpolecat" %% "atto-core" % "0.7.0",
  "org.tpolecat" %% "atto-refined" % "0.7.0"
).map(_.cross(CrossVersion.for3Use2_13))

libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.4.1"
)
