name := """Chip8-Emulator"""

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.lwjgl" % "lwjgl-platform" % "3.0.0a" classifier "natives-windows" classifier "natives-linux" classifier "natives-osx",
  "org.lwjgl" % "lwjgl" % "3.0.0a",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"
)

lazy val nativesExtract = TaskKey[Unit]("natives-extract", "Extracts LWJGL Native JAR file")

def defineOs = System.getProperty("os.name").toLowerCase.take(3).toString match {
  case "lin" => ("linux", "so")
  case "mac" | "dar" => ("osx", "lib")
  case "win" => ("windows", "dll")
  case "sun" => ("solaris", "so")
  case _ => ("unknown", "")
}

def extractLwjglNativesTask : Def.Initialize[Task[Unit]] = (streams, classpathTypes, update) map {
  (s, ct, u) =>
    val natives = Classpaths.managedJars(Compile, ct, u) map { _.data } find( (jar) => jar.getName.startsWith("lwjgl-platform") && jar.getName.endsWith(s"${defineOs._1}.jar"))
    natives map {
      val target = file(".") / "libs" / "natives"
      s.log.info(s"Extracting LWJGL natives to $target")
      IO.unzip(_, target)
    } getOrElse {
      s.log.warn("Unable to find LWJGL natives jar. Try update again.")
    }
  }

nativesExtract <<= extractLwjglNativesTask

compile <<= (compile in Compile) dependsOn(nativesExtract)

initialCommands := ""
