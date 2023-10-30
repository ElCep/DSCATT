


//resolvers += "Github packages" at "https://gitlab.inria.fr/api/v4/projects/39367/packages/maven/"
//scalacOptions ++= Seq("-Dmaven.wagon.http.ssl.insecure=true -Dmaven.wagon.http.ssl.allowall=true")

//libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.2"

lazy val circe = Seq(
  libraryDependencies += "org.json4s" %% "json4s-jackson" % "4.0.6",
  libraryDependencies += "io.circe" %% "circe-generic" % "0.14.5",
  libraryDependencies += "org.http4s" %% "http4s-circe" % "0.23.19",
)

lazy val betterFile = libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.2"

lazy val dscatt = project.in(file("dscatt")) enablePlugins (SbtOsgi) settings(
  name := "DSCATT",
  scalaVersion := "3.3.0",

  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % "0.14.5"),
  betterFile,
  osgiSettings,
  OsgiKeys.exportPackage := Seq("swing.*;-split-package:=merge-first"),
  OsgiKeys.importPackage := Seq("*;resolution:=optional"),
  OsgiKeys.privatePackage := Seq("!scala.*,META-INF.*,*"),
  OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.14))"""",
  mainClass in(Compile, run) := Some("dscatt.Diohine")
) dependsOn(data)

lazy val parcelGenerator = project.in(file("parcelGenerator")) enablePlugins (SbtOsgi) settings(
  name := "Parcellor",
  scalaVersion := "3.3.0",
  libraryDependencies += "fr.ign.artiscales.pm" % "ParcelManager" % "1.4-SNAPSHOT" from "https://gitlab.inria.fr/api/v4/projects/39367/packages/maven/fr/ign/artiscales/pm/ParcelManager/1.4.1/ParcelManager-1.4.1-jar-with-dependencies.jar",
  betterFile,
  circe,
  mainClass in(Compile, run) := Some("parcelgenerator.App")
) dependsOn(data)


lazy val data = project.in(file("shared")) settings (
  name := "data",
  scalaVersion := "3.3.0"
)