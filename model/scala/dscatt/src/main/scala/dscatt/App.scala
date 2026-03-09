package dscatt

import dscatt.launchers.commonSettings.*

object Diohine:

  def main(args: Array[String]) =

    val landsDirectory = java.io.File(args.head + "/s777k22g0,20.json")
    //dscatt.launchers.ChangeOverTime.run(seed.toLong, landsDirectory)

    //checkGrowth
    dscatt.launchers.Unitary.run(seed.toLong, landsDirectory)
    // dscatt.launchers.Replicate.run(1000, landsDirectory)