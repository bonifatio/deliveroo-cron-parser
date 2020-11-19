package uk.co.deliveroo

import cats.Show
import cats.implicits._
import uk.co.deliveroo.PatternKind.{parseMonthFragment, parseWeekDayFragment, parseYearsFragment}

final case class CrontabItem(minutes: List[Int],
                             hours: List[Int],
                             dayOfMonth: List[Int],
                             month: List[Int],
                             dayOfWeek: List[Int],
                             year: Option[List[Int]],
                             command: String
                            )

object CrontabItem {

  def parseYearAndCommand(s: String): (Option[List[Int]], String) = {
    val parts = s.split("\\s", 2)

    if(parts.length == 1) {
      (None, s)
    } else {
      val Array(year, command) = parts

      parseYearsFragment(year).flatMap(_.resolveYears) match {
        case Left(_) =>
          (None, s)

        case Right(v) =>
          (Some(v), command)
      }
    }
  }

  def parse(s: String): Either[String, CrontabItem] = {

    val trimmed = s.trim

    trimmed.split("\\s", 6) match {
      case Array(m, h, day, month, weekDay, extra) =>
        import PatternKind.parseNumericFragment

        for {
          minutes <- parseNumericFragment(m) flatMap { _.resolveMinutes }
          hours <- parseNumericFragment(h) flatMap { _.resolveHours }
          daysOfMonth <- parseNumericFragment(day) flatMap { _.resolveDaysOfMonth }
          months <- parseNumericFragment(month) orElse parseMonthFragment(month) flatMap { _.resolveMonths }
          daysOfWeek <- parseNumericFragment(weekDay) orElse parseWeekDayFragment(weekDay) flatMap { _.resolveDaysOfWeek }
          (year, command) = parseYearAndCommand(extra)
        } yield {
          CrontabItem(
            minutes,
            hours,
            daysOfMonth,
            months,
            daysOfWeek,
            year,
            command
          )
        }

      case _ =>
        s"Not enough parts in crontab string '$trimmed'".asLeft
    }
  }

  val PaddingLength = 14

  implicit val showCrontabItem: Show[CrontabItem] = Show.show { item =>
    val parts = Seq(
      "minute" -> item.minutes,
      "hour" -> item.hours,
      "day of month" -> item.dayOfMonth,
      "month" -> item.month,
      "day of week" -> item.dayOfWeek
    )

    val partsWithYear =
      if(item.year.isEmpty) parts else parts :+ ("year" -> item.year.get)

    val timeMappings = partsWithYear map { case (n, v) =>
      n -> v.mkString(" ")
    }

    val lines = for {
      (name, value) <- timeMappings :+ ("command" -> item.command)
    } yield {
      name.padTo(PaddingLength, " ").mkString + value
    }

    lines mkString "\r\n"
  }
}
