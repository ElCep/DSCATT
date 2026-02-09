////> using scala 3.8.1
////> using dep org.apache.commons:commons-math3:3.6.1
////> using dep io.github.json4s::json4s-jackson:4.1.0
package jsonconverter

import ujson._
import scala.io.Source

object JSONConverter

  :
  @main def run =


    val source = Source.fromFile("/home/mathieu/work/models/results/DSCATT/popRainFallDecrease.json")

    val json = ujson.read(source.mkString)

    val variablesNode =
      json("data")(0)("variables")


    val popDyn: Array[Array[Int]] =
      variablesNode("populationDynamic").arr.map { inner =>
        inner.arr.map(_.num.toInt).toArray
      }.toArray

    val initialK: Array[Int] = variablesNode("initialNumberOfKitchensP1").arr.map(_.num.toInt).toArray
    val uniqueValueMap = initialK.toSeq.zipWithIndex.groupBy(_._1).map(x => x._1 -> x._2.map(_._2))

    val rainFallDecreaseRates: Array[Double] = variablesNode("rainFallDecreaseRate").arr.map(_.num).toArray
    val rainFallStages = Seq(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0)
    val rangeValueMap =
      rainFallDecreaseRates.toSeq.zipWithIndex.groupBy: rf =>
        rainFallStages.lastIndexWhere(_ < rf._1)
      .map(x=> x._1-> x._2.map(_._2))


    val popKP10 = uniqueValueMap.getOrElse(3, Seq()).map(popDyn(_))
    val popKP20 = uniqueValueMap.getOrElse(6, Seq()).map(popDyn(_))
    val popKP30 = uniqueValueMap.getOrElse(9, Seq()).map(popDyn(_))
    val popKP40 = uniqueValueMap.getOrElse(12, Seq()).map(popDyn(_))
    val popKP50 = uniqueValueMap.getOrElse(15, Seq()).map(popDyn(_))

    val rfrU65 = rangeValueMap.getOrElse(0, Seq()).map(popDyn(_))
    val rfrU70 = rangeValueMap.getOrElse(1, Seq()).map(popDyn(_))
    val rfrU75 = rangeValueMap.getOrElse(2, Seq()).map(popDyn(_))
    val rfrU80 = rangeValueMap.getOrElse(3, Seq()).map(popDyn(_))
    val rfrU85 = rangeValueMap.getOrElse(4, Seq()).map(popDyn(_))
    val rfrU90 = rangeValueMap.getOrElse(5, Seq()).map(popDyn(_))
    val rfrU95 = rangeValueMap.getOrElse(6, Seq()).map(popDyn(_))
    val rfrU1 = rangeValueMap.getOrElse(7, Seq()).map(popDyn(_))

    val content =
      ujson.Obj(
        "popKP10" -> ujson.Arr.from(popKP10.map(inner => ujson.Arr.from(inner))),
        "popKP20" -> ujson.Arr.from(popKP20.map(inner => ujson.Arr.from(inner))),
        "popKP30" -> ujson.Arr.from(popKP30.map(inner => ujson.Arr.from(inner))),
        "popKP40" -> ujson.Arr.from(popKP40.map(inner => ujson.Arr.from(inner))),
        "popKP50" -> ujson.Arr.from(popKP50.map(inner => ujson.Arr.from(inner))),
        "rfru65" -> ujson.Arr.from(rfrU65.map(inner => ujson.Arr.from(inner))),
        "rfru70" -> ujson.Arr.from(rfrU70.map(inner => ujson.Arr.from(inner))),
        "rfru75" -> ujson.Arr.from(rfrU75.map(inner => ujson.Arr.from(inner))),
        "rfru80" -> ujson.Arr.from(rfrU80.map(inner => ujson.Arr.from(inner))),
        "rfru85" -> ujson.Arr.from(rfrU85.map(inner => ujson.Arr.from(inner))),
        "rfru90" -> ujson.Arr.from(rfrU90.map(inner => ujson.Arr.from(inner))),
        "rfru95" -> ujson.Arr.from(rfrU95.map(inner => ujson.Arr.from(inner))),
        "rfru1" -> ujson.Arr.from(rfrU1.map(inner => ujson.Arr.from(inner)))
      ).render(indent = 2)

    import java.nio.file.{Files, Paths}
    import java.nio.charset.StandardCharsets

    val path = Paths.get("/home/mathieu/Bureau/aa.json")

    Files.write(path, content.getBytes(StandardCharsets.UTF_8))