
scalaVersion := "3.3.0"

name := "Diohine"

enablePlugins(SbtOsgi)
OsgiKeys.exportPackage := Seq("swing.*;-split-package:=merge-first")
OsgiKeys.importPackage := Seq("*;resolution:=optional")
OsgiKeys.privatePackage := Seq("!scala.*,META-INF.*,*")
OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.14))""""

osgiSettings

mainClass in(Compile, run) := Some("dscatt.Diohine")

//resolvers += "Github packages" at "https://gitlab.inria.fr/api/v4/projects/39367/packages/maven/"
//scalacOptions ++= Seq("-Dmaven.wagon.http.ssl.insecure=true -Dmaven.wagon.http.ssl.allowall=true")

libraryDependencies += "fr.ign.artiscales.pm" % "ParcelManager" % "1.3-SNAPSHOT" from "https://gitlab.inria.fr/api/v4/projects/39367/packages/maven/fr/ign/artiscales/pm/ParcelManager/1.3-SNAPSHOT/ParcelManager-1.3-20220729.152938-7-shaded.jar"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.2"