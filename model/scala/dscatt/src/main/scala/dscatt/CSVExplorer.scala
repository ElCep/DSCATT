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

    // QS do not increase much more than 10% and is not monotonic
    def getMaxLast10PCAndNotSequenceMax: RowData =
      val content =
        rowData.content.filter: line =>
          val qs = line(20).tail.dropRight(1).split(",").map(_.toDouble)
          val max = qs.max
          qs.head * 1.05 >= qs.last && max != qs.last
      rowData.copy(content = content)

    def closestTo700Yield =
      val sort = rowData.content.sortBy(c=> math.abs(c(21).toDouble - 700))
      rowData.copy(content = Seq(sort.head))

    def printObjectivesAndCo =
      rowData.content.foreach: line=>
        println(line(2) + " - " + line(3) + " - "  + line(4) + " - "  + line(5) + " - "  + line(6) + " - " + line(21) + " - " + line(14))

    def printQS =
      rowData.content.foreach: line=>
        val qs = line(20).tail.dropRight(1).split(",").map(_.toDouble)
        println(qs.toSeq)

    def printEF17andLast =
      rowData.content.foreach: line=>
        val ef = line(15).tail.dropRight(1).split(",").map(_.toDouble)
        println(ef(17) + " - " + ef.last)

    def printPop =
      rowData.content.foreach: line =>
        val pop = line(18).tail.dropRight(1).split(",").map(_.toDouble).toSeq
        println(pop)

  def run =
    val content = scala.io.Source.fromFile("/tmp/out.csv").getLines().toSeq
    val rowData = fromCSV(content)
    val result = rowData.getNullAgroObjective.getMaxLast10PCAndNotSequenceMax.closestTo700Yield

    //result.printEF17andLast
    //result.printObjectivesAndCo
    println("# " + result.content.length)
    println("closest " + result.printObjectivesAndCo)



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
