package dscatt

object Diohine{

  case class Parameters(outputParcelPath: Option[String] = None)


  def main(args: Array[String]) = {
    val parameters = if (args.length > 0) {
      Parameters(Some(args(0)))
    } else Parameters()

    Simulation(12, 0.5, 0.01, 200, parameters.outputParcelPath)
  }

}

