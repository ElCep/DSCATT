


//resolvers += "Github packages" at "https://gitlab.inria.fr/api/v4/projects/39367/packages/maven/"
//scalacOptions ++= Seq("-Dmaven.wagon.http.ssl.insecure=true -Dmaven.wagon.http.ssl.allowall=true")

//libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.2"

val ScalaVersion = "3.3.1"

lazy val circe = Seq(
  libraryDependencies += "org.json4s" %% "json4s-jackson" % "4.0.6",
  libraryDependencies += "io.circe" %% "circe-generic" % "0.14.5",
  libraryDependencies += "org.http4s" %% "http4s-circe" % "0.23.19",
)

lazy val betterFile = libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.2"

lazy val dscatt = project.in(file("dscatt")) enablePlugins (SbtOsgi) settings(
  name := "DSCATT",
  scalaVersion := ScalaVersion,

  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % "0.14.5"),
  betterFile,
  osgiSettings,
  OsgiKeys.exportPackage := Seq("dscatt.*"),
  OsgiKeys.importPackage := Seq("*;resolution:=optional"),
  OsgiKeys.privatePackage := Seq("!scala.*","*"),
  OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))"""",
  Compile / run / mainClass := Some("dscatt.Diohine")
) dependsOn(data)

lazy val parcelGenerator = project.in(file("parcelGenerator")) settings(
  name := "Parcellor",
  scalaVersion := ScalaVersion,
  libraryDependencies += "fr.ign.artiscales.pm" % "ParcelManager" % "1.4.2" from "https://gitlab.inria.fr/api/v4/projects/39367/packages/maven/fr/ign/artiscales/pm/ParcelManager/1.4.2/ParcelManager-1.4.2-shaded.jar ",
  betterFile,
  circe,
  Compile / run / mainClass := Some("parcelgenerator.App")
) dependsOn(data)


lazy val data = project.in(file("shared")) settings (
  name := "data",
  scalaVersion := ScalaVersion
)