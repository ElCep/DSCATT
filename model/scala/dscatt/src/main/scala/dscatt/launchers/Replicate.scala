package dscatt.launchers

object Replicate:

  def run(iterations: Int, landsDirectory: java.io.File) =
    println("seed,ef,pop,yield,herd")
    for i <- 1 to iterations
      do Unitary.run(i.toLong, java.io.File(s"landsDirectory/s{$i}k22g0,20.json"))
