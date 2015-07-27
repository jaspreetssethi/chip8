name := """Chip8-Emulator"""

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.lwjgl" % "lwjgl" % "3.0.0a",
  "org.lwjgl" % "lwjgl-platform" % "3.0.0a" classifier "natives-windows" classifier "natives-linux" classifier "natives-osx",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"
)

initialCommands := ""
