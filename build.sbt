crossScalaVersions := Seq("2.12.3-bin-typelevel-4", "2.11.11-bin-typelevel-4")

organization in ThisBuild := "com.thoughtworks.compute"

lazy val Memory = project

lazy val Closeables = project

lazy val OpenCL = project.dependsOn(Closeables, Memory)

lazy val OpenCLCodeGenerator = project.dependsOn(Memory)

lazy val Benchmark = project.dependsOn(OpenCL)

scalaOrganization in ThisBuild := "org.typelevel"
