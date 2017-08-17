val lwjglNatives: String = {
  if (util.Properties.isMac) {
    "natives-macos"
  } else if (util.Properties.osName.startsWith("Linux")) {
    "natives-linux"
  } else if (util.Properties.isWin) {
    "natives-windows"
  } else {
    throw new MessageOnlyException(s"lwjgl does not support ${util.Properties.osName}")
  }
}

libraryDependencies += "org.lwjgl" % "lwjgl-opencl" % "3.1.2"

libraryDependencies += "org.lwjgl" % "lwjgl" % "3.1.2"

libraryDependencies += "org.lwjgl" % "lwjgl" % "3.1.2" % Test classifier lwjglNatives

libraryDependencies += "com.thoughtworks.raii" %% "asynchronous" % "3.0.0-M0"

libraryDependencies += "com.thoughtworks.feature" %% "partialapply" % "2.0.1"

libraryDependencies += "com.thoughtworks.feature" %% "implicitapply" % "2.0.1"

libraryDependencies += "com.thoughtworks.feature" %% "factory" % "2.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % Test

fork := true

scalacOptions += "-Ypartial-unification"

libraryDependencies += "com.thoughtworks.each" %% "each" % "3.3.1" % Test

enablePlugins(Example)
