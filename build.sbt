ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "Hatla2ee_chatbot"
  )
libraryDependencies ++= Seq(
  "edu.stanford.nlp" % "stanford-corenlp" % "4.5.7",
  "edu.stanford.nlp" % "stanford-corenlp" % "4.5.7" classifier "models")