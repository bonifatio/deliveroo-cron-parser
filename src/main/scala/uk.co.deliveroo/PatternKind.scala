package uk.co.deliveroo

import cats.implicits._

sealed trait PatternKind

final case class Wildcard(step: Option[Int]) extends PatternKind

final case class SingleValue(value: Int) extends PatternKind

final case class ValuesRange(from: Int, to: Int) extends PatternKind

final case class ValuesList(values: List[Int]) extends PatternKind

object PatternKind {

  val wildcardRe = "^\\*(/(\\d{1,2}))?$".r

  val singleNumberRe = "^(\\d{1,2})$".r
  val numericRangeRe = "^(\\d{1,2})-(\\d{1,2})$".r
  val numericListRe = "^(\\d{1,2})(,\\d{1,2})+$".r

  val singleYearRe = "^(\\d{4})$".r
  val yearsRangeRe = "^(\\d{4})-(\\d{4})$".r
  val yearsListRe = "^(\\d{4})(,\\d{4})+$".r

  val weeks = "(sun|mon|tue|wed|thu|fri|sat)"
  val singleWeekdayRe = s"^$weeks$$".r
  val weekdayRangeRe = s"^$weeks-$weeks$$".r
  val weekdayListRe = s"^$weeks(,$weeks)+$$".r

  val months = "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)"
  val singleMonthRe = s"^$months$$".r
  val monthRangeRe = s"^$months-$months$$".r
  val monthListRe = s"^$months(,$months)+$$".r

  val weekdaysLookup = Seq(
    "sun", "mon", "tue", "wed", "thu", "fri", "sat"
  ).zipWithIndex.toMap

  val monthsLookup = Seq(
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec"
  ).zipWithIndex.toMap.view.mapValues(_ + 1).toMap

  def parseNumericFragment(s: String): Either[String, PatternKind] = {

    s match {
      case singleNumberRe(v) =>
        SingleValue(v.toInt).asRight

      case wildcardRe(_, s) =>
        val step = Option(s).map(_.toInt)
        Wildcard(step).asRight

      case numericRangeRe(start, end) =>
        ValuesRange(start.toInt, end.toInt).asRight

      case numericListRe(_*) =>
        val values = s.split(',').map(_.toInt).toList
        ValuesList(values).asRight

      case _ =>
        s"Cannot parse pattern '$s'".asLeft
    }
  }

  def parseYearsFragment(s: String): Either[String, PatternKind] = {

    s match {
      case singleYearRe(v) =>
        SingleValue(v.toInt).asRight

      case yearsRangeRe(start, end) =>
        ValuesRange(start.toInt, end.toInt).asRight

      case yearsListRe(_*) =>
        val values = s.split(',').map(_.toInt).toList
        ValuesList(values).asRight

      case _ =>
        s"Cannot parse pattern '$s'".asLeft
    }
  }

  def parseWeekDayFragment(s: String): Either[String, PatternKind] = {

    s match {
      case singleWeekdayRe(v) =>
        SingleValue(weekdaysLookup(v)).asRight

      case weekdayRangeRe(start, end) =>
        ValuesRange(weekdaysLookup(start), weekdaysLookup(end)).asRight

      case weekdayListRe(_*) =>
        val values = s.split(',').map(weekdaysLookup).toList
        ValuesList(values).asRight

      case _ =>
        s"Cannot parse pattern '$s'".asLeft
    }
  }

  def parseMonthFragment(s: String): Either[String, PatternKind] = {

    s match {
      case singleMonthRe(v) =>
        SingleValue(monthsLookup(v)).asRight

      case monthRangeRe(start, end) =>
        ValuesRange(monthsLookup(start), monthsLookup(end)).asRight

      case monthListRe(_*) =>
        val values = s.split(',').map(monthsLookup).toList
        ValuesList(values).asRight

      case _ =>
        s"Cannot parse pattern '$s'".asLeft
    }
  }

  implicit class PatternKindOps(private val self: PatternKind) extends AnyVal {

    private def resolveDateTimePart(pattern: PatternKind,
                                    minValue: Int,
                                    maxValue: Int): Either[String, List[Int]] = {

      def withinRange(v: Int) =
        (v >= minValue) && (v <= maxValue)

      pattern match {
        case Wildcard(Some(step)) =>
          Either.cond(
            step > 0,
            (minValue to maxValue by step).toList,
            s"Invalid step '$step'"
          )

        case Wildcard(None) =>
          (minValue to maxValue).toList.asRight

        case SingleValue(value) =>
          Either.cond(
            withinRange(value),
            List(value),
            s"Value '$value' is not within [$minValue, $maxValue] range"
          )

        case ValuesRange(fromValue, toValue) =>
          Either.cond(
            withinRange(fromValue) && withinRange(toValue) && (fromValue < toValue),
            (fromValue to toValue).toList,
            s"Invalid [$fromValue, $toValue] range"
          )

        case ValuesList(values) =>
          val invalidItems = values.filterNot(withinRange)

          Either.cond(
            invalidItems.isEmpty,
            values.sorted,
            s"Value(s) '${invalidItems.mkString(",")}' are not within [$minValue, $maxValue] range"
          )
      }
    }

    def resolveMinutes: Either[String, List[Int]] =
      resolveDateTimePart(self, 0, 59)

    def resolveHours: Either[String, List[Int]] =
      resolveDateTimePart(self, 0, 23)

    def resolveDaysOfMonth: Either[String, List[Int]] =
      resolveDateTimePart(self, 1, 31)

    def resolveMonths: Either[String, List[Int]] =
      resolveDateTimePart(self, 1, 12)

    def resolveDaysOfWeek: Either[String, List[Int]] =
      resolveDateTimePart(self, 0, 6)

    def resolveYears: Either[String, List[Int]] =
      resolveDateTimePart(self, 1970, 9999)
  }
}
