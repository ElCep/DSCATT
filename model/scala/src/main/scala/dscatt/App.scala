package dscatt

object Diohine extends App {

  val diohine = World.buildWorldGeometry(12, 0.5, 0.01, 200, Some("/tmp/out.gpkg"))
  World.display(diohine)
  
}

