name := "MathLogic"

version := "1.0"

//org.scalastyle.sbt.ScalastylePlugin.Settings

//libraryDependencies ++= Seq("org.specs2" %% "specs2" % "2.4.2" % "test")

//libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.0.1"

scalacOptions in Test ++= Seq("-Yrangepos")

// Read here for optional dependencies:
// http://etorreborre.github.io/specs2/guide/org.specs2.guide.Runners.html#Dependencies

scalacOptions ++= Seq("-feature")

initialCommands in console := "import com.volhovm.mathlogic.propositional._; import IOUtil._"

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)
