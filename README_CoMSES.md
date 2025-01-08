# Preamble

This model doesn't use any ABM framework.
It is written in Scala and is intended to be run inside a terminal, via the comand line interface.

This readme file is for executing the code contained in the CoMSES file structrure. 
For a more traditional use of the code, go to https://github.com/ElCep/DSCATT/tree/fertility


# Installation

Requirements :

    sbt : https://www.scala-sbt.org/
    a java JDK version >= 11

# Running the model 

Inside the code directory, go to DSCATT/model/scala and launch the `sbt` tool

You should have a prompt looking like this : sbt:Diohine>

The model generates a geopackage file containing the parcels geometries, you have to provide an absolute path to store it after the run command :

run /absolute/path/to/my_parcels.gpkg

# Getting the outputs files 

TODO

