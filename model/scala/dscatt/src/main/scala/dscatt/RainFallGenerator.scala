package dscatt

object RainFallGenerator {

  val realDataRainFall = Seq(623, 404, 408, 388, 729, 620, 528, 394, 484, 395, 635, 540, 526, 652, 691, 720, 416, 723, 536, 353, 767, 527, 509, 501)

  val rng = util.Random(77L)

  // 30 year
  def thirtyPercentLess =
    for
      i <- 0 to 29
      rf = realDataRainFall(rng.nextInt(24)) * 0.8
    yield rf.toInt


}
