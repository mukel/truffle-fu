name := "truffle-fu"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.oracle.truffle" % "truffle-api" % "0.18",
  "com.lihaoyi" %% "fastparse" % "1.0.0",

  // Java bytecode.
  // https://mvnrepository.com/artifact/org.cojen/cojen
  "org.cojen" % "cojen" % "2.2.5"
)