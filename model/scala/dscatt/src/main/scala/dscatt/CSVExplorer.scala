package dscatt

import scala.annotation.tailrec

object CSVExplorer:

  case class RowData(headers: Seq[String] = Seq(), content: Seq[Seq[String]] = Seq(), dimensions: Seq[Int])

  implicit class RowDataDecorator(rowData: RowData):

    def getNullAgroObjective: RowData =
      val content =
        rowData.content.filter: line =>
          line(8) == "0.0"
      rowData.copy(content = content)

    def get20Replications =
      val content =
        rowData.content.filter: line =>
          line(9) == "20"
      rowData.copy(content = content)

    def filterFallowBoost =
      val content =
        rowData.content.filter: line =>
          val fb = line(4).toDouble
          fb > 0.8 && fb < 1.0
      rowData.copy(content = content)

    // QS do not increase much more than 10% and is not monotonic
    def fivePCTube: RowData =
      val content =
        rowData.content.filter: line =>
          val qs = toAADouble(line(20)).transpose.map(_.sum / 20)
          val maxTube = qs.head * 1.02
          val minTube = qs.head * 0.85
          qs.forall(el=> el <= maxTube && el >= minTube)
      rowData.copy(content = content)

    def around700Yield =
      val sort = rowData.content.filter: line=>
        val y = toAADouble(line(14)).transpose
        val yy= y.map(x=> x.sum / x.length)
        val averageYield = yy.sum / yy.length
        println("AY " + averageYield)
        averageYield >= 670 && averageYield <= 730
      rowData.copy(content = sort)

    def printObjectivesAndCo =
      rowData.content.foreach: line=>
        println(line(2) + " & " + line(3) + " & "  + line(4) + " & "  + line(5) + " & "  + line(6) + " & "  + line(7) + " & "  + line(8) )//+ " - " + line(22) + " - " + line(15))

    def printMeanHerdSize = printMean(14)
    def printMeanEffectiveFallow = printMean(15)
    def printMeanAnnualQS = printMean(20)
    def printMeanMilletYield = printMean(22)
    def printMeanPopulation = printMean(19)

    def printMean(col: Int) =
        rowData.content.foreach: line =>
          val x0 = toAADouble(line(col)).transpose
          val x = x0.map(x => x.sum / x.length)
          println(x.toSeq.tail.length)
          println(x.toSeq.tail)

    def printEF17andLast =
      rowData.content.foreach: line=>
        val ef = line(16).tail.dropRight(1).split(",").map(_.toDouble)
        println(ef(17) + " - " + ef.last)

    def printPop =
      rowData.content.foreach: line =>
        val pop = line(19).tail.dropRight(1).split(",").map(_.toDouble).toSeq
        println(pop)

  def run =
    val content = scala.io.Source.fromFile("/tmp/realMeteoMedian.csv").getLines().toSeq
    val rowData = fromCSV(content)

    val result = rowData.get20Replications//.getNullAgroObjective.filterFallowBoost//.fivePCTube//.around700Yield

 //   result.printMeanHerdSize
 //   result.printMeanEffectiveFallow
//    result.printMeanAnnualQS
//    result.printMeanPopulation
 //   result.printMeanMilletYield

    result.printObjectivesAndCo
    println("# " + result.content.length)
//    println("closest " + result.printObjectivesAndCo)

  def toADouble(s: String) = s.tail.dropRight(1).split(",").map(_.toDouble)

  def toAADouble(s: String) = s.tail.dropRight(2).split("],").map(_.tail.split(",").map(_.toDouble))

  def fromCSV(lines: Seq[String]) = {

    trait Pending
    case class ArrayPending(string: String, balance: Int) extends Pending
    case class QuotePending(string: String, balance: Int) extends Pending
    case class RegularPending(string: String) extends Pending

    def balance(s: String) = s.count(_ == '[') - s.count(_ == ']')

    case class ParsingException(message: String) extends Exception(message)

    def dimension(s: String): Int =
      @tailrec
      def dimension0(toBeParsed: String, dim: Int): Int =
        if (toBeParsed.isEmpty) dim
        else
          val cur = toBeParsed.head
          cur match
            case ']' => dim
            case '[' => dimension0(toBeParsed.tail, dim + 1)
            case _ => dimension0(toBeParsed.tail, dim)

      dimension0(s, 0)


    @tailrec
    def parse0(toBeChecked: String, pending: Option[Pending], parsed: List[String]): Seq[String] = {
      if (toBeChecked.isEmpty) {
        pending match {
          case Some(ar: ArrayPending) => if (ar.balance == 0) parsed :+ ar.string else throw ParsingException("Unfinished array")
          case Some(rp: RegularPending) => parsed :+ rp.string
          case _ => parsed
        }
      } else {
        val currentChar = toBeChecked.head
        val tail = toBeChecked.tail
        currentChar match {
          case ',' => pending match {
            case Some(rp: RegularPending) => parse0(tail, None, parsed :+ rp.string)
            case Some(ap: ArrayPending) =>
              if (ap.balance == 0) parse0(tail, None, parsed :+ ap.string)
              else parse0(tail, Some(ap.copy(ap.string :+ currentChar)), parsed)
            case _ => throw ParsingException("Coma set in bad position")
          }
          case '[' | ']' =>
            pending match {
              case Some(ap: ArrayPending) =>
                val newString = ap.string + currentChar
                parse0(tail, Some(ap.copy(string = newString, balance(newString))), parsed)
              case Some(rp: RegularPending) => throw ParsingException("'[' or ']' set in bad position")
              case _ => currentChar match {
                case '[' => parse0(tail, Some(ArrayPending(currentChar.toString, balance = 1)), parsed)
                case _ => throw ParsingException("']' set in bad position")
              }
            }
          case ' ' | '"' => parse0(tail, pending, parsed)
          case c: Char => pending match {
            case Some(ap: ArrayPending) => parse0(tail, Some(ap.copy(string = ap.string :+ c)), parsed)
            case Some(rp: RegularPending) => parse0(tail, Some(rp.copy(string = rp.string :+ c)), parsed)
            case _ => parse0(tail, Some(RegularPending(c.toString)), parsed)
          }
        }
      }
    }

    val header: Seq[String] = lines.head.split(",").toSeq
    val body = lines.tail.map(line => parse0(line, None, List()))
    val dims = body.headOption.map {
      _.map { s => dimension(s) }
    }.toSeq.flatten

    RowData(header, body, dims)
  }
