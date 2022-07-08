package dscatt

object Simulation {

  def apply(
             numberOfKitchen: Int,
             giniParcels: Double,
             giniTolerance: Double = 0.01,
             maximumNumberOfParcels: Int = 200,
             parcelOutputPath: Option[String] = None
           ) = {

    val diohine = World.buildWorldGeometry(12, 0.5, 0.01, 200, Some("/tmp/out.gpkg"))
    World.display(diohine)
  }
}
