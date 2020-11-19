package uk.co.deliveroo

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.EitherValues._

class PatternKindSpec extends FlatSpec with Matchers {

  import PatternKind._

  behavior of "when working with numeric patterns"

  it should "parse valid single value" in {
    parseNumericFragment("24").right.value shouldBe SingleValue(24)
  }

  it should "parse valid range" in {
    parseNumericFragment("24-48").right.value shouldBe ValuesRange(24, 48)
  }

  it should "parse valid list" in {
    parseNumericFragment("16,8,10").right.value shouldBe ValuesList(List(16, 8, 10))
  }

  it should "parse valid wildcards" in {
    parseNumericFragment("*").right.value shouldBe Wildcard(None)
    parseNumericFragment("*/9").right.value shouldBe Wildcard(Some(9))
  }

  it should "fail for invalid strings" in {
    parseNumericFragment("abc").isLeft shouldBe true
    parseNumericFragment("100").isLeft shouldBe true
    parseNumericFragment("10-234").isLeft shouldBe true
    parseNumericFragment("*/102").isLeft shouldBe true
  }

  behavior of "when working with weekday patterns"

  it should "parse valid single value" in {
    parseWeekDayFragment("sat").right.value shouldBe SingleValue(6)
  }

  it should "parse valid range" in {
    parseWeekDayFragment("tue-fri").right.value shouldBe ValuesRange(2, 5)
  }

  it should "parse valid list" in {
    parseWeekDayFragment("mon,sun,thu").right.value shouldBe ValuesList(List(1, 0, 4))
  }

  it should "fail for invalid strings" in {
    parseWeekDayFragment("abc").isLeft shouldBe true
    parseWeekDayFragment("friday").isLeft shouldBe true
  }

  behavior of "when working with month patterns"

  it should "parse valid single value" in {
    parseMonthFragment("aug").right.value shouldBe SingleValue(8)
  }

  it should "parse valid range" in {
    parseMonthFragment("apr-sep").right.value shouldBe ValuesRange(4, 9)
  }

  it should "parse valid list" in {
    parseMonthFragment("oct,mar,dec").right.value shouldBe ValuesList(List(10, 3, 12))
  }

  it should "fail for invalid strings" in {
    parseMonthFragment("abc").isLeft shouldBe true
    parseMonthFragment("july").isLeft shouldBe true
  }

  behavior of "when working with PatternKind#resolveXYZ extension methods"

  it should "resolve minutes" in {
    SingleValue(8).resolveMinutes.right.value shouldBe List(8)
    ValuesRange(10, 15).resolveMinutes.right.value shouldBe List(10, 11, 12, 13, 14, 15)
    Wildcard(None).resolveMinutes.right.value shouldBe (0 to 59).toList
    Wildcard(Some(12)).resolveMinutes.right.value shouldBe List(0, 12, 24, 36, 48)
    ValuesList(List(34, 9, 15)) .resolveMinutes.right.value shouldBe List(9, 15, 34)
  }

  it should "resolve hours" in {
    SingleValue(11).resolveHours.right.value shouldBe List(11)
    ValuesRange(5, 8).resolveHours.right.value shouldBe List(5, 6, 7, 8)
    Wildcard(None).resolveHours.right.value shouldBe (0 to 23).toList
    Wildcard(Some(4)).resolveHours.right.value shouldBe List(0, 4, 8, 12, 16, 20)
    ValuesList(List(22, 9, 11)) .resolveHours.right.value shouldBe List(9, 11, 22)
  }

  it should "resolve months" in {
    SingleValue(9).resolveMonths.right.value shouldBe List(9)
    ValuesRange(5, 8).resolveMonths.right.value shouldBe List(5, 6, 7, 8)
    Wildcard(None).resolveMonths.right.value shouldBe (1 to 12).toList
    Wildcard(Some(5)).resolveMonths.right.value shouldBe List(1, 6, 11)
    ValuesList(List(7, 6, 11)) .resolveMonths.right.value shouldBe List(6, 7, 11)
  }

  it should "resolve days of month" in {
    SingleValue(19).resolveDaysOfMonth.right.value shouldBe List(19)
    ValuesRange(5, 8).resolveDaysOfMonth.right.value shouldBe List(5, 6, 7, 8)
    Wildcard(None).resolveDaysOfMonth.right.value shouldBe (1 to 31).toList
    Wildcard(Some(11)).resolveDaysOfMonth.right.value shouldBe List(1, 12, 23)
    ValuesList(List(7, 6, 11)) .resolveDaysOfMonth.right.value shouldBe List(6, 7, 11)
  }

  it should "resolve week days" in {
    SingleValue(3).resolveDaysOfWeek.right.value shouldBe List(3)
    ValuesRange(2, 5).resolveDaysOfWeek.right.value shouldBe List(2, 3, 4, 5)
    Wildcard(None).resolveDaysOfWeek.right.value shouldBe (0 to 6).toList
    Wildcard(Some(3)).resolveDaysOfWeek.right.value shouldBe List(0, 3, 6)
    ValuesList(List(2, 6, 5)).resolveDaysOfWeek.right.value shouldBe List(2, 5, 6)
  }

  it should "fail to resolve for invalid inputs" in {
    SingleValue(64).resolveMinutes.isLeft shouldBe true
    ValuesRange(15, 10).resolveMinutes.isLeft shouldBe true
    Wildcard(Some(0)).resolveMinutes.isLeft shouldBe true
    ValuesList(List(78, 9, 15)).resolveMinutes.isLeft shouldBe true

    SingleValue(26).resolveHours.isLeft shouldBe true
    ValuesRange(15, 10).resolveHours.isLeft shouldBe true
    ValuesRange(25, 30).resolveHours.isLeft shouldBe true
    Wildcard(Some(0)).resolveHours.isLeft shouldBe true
    ValuesList(List(78, 9, 15)).resolveHours.isLeft shouldBe true

    SingleValue(36).resolveDaysOfMonth.isLeft shouldBe true
    ValuesRange(5, 2).resolveDaysOfMonth.isLeft shouldBe true
    ValuesRange(2, 42).resolveDaysOfMonth.isLeft shouldBe true
    Wildcard(Some(0)).resolveDaysOfMonth.isLeft shouldBe true
    ValuesList(List(8, 42, 15)).resolveDaysOfMonth.isLeft shouldBe true

    SingleValue(26).resolveDaysOfWeek.isLeft shouldBe true
    ValuesRange(6, 2).resolveDaysOfWeek.isLeft shouldBe true
    ValuesRange(2, 7).resolveDaysOfWeek.isLeft shouldBe true
    Wildcard(Some(0)).resolveDaysOfWeek.isLeft shouldBe true
    ValuesList(List(1, 4, 8)).resolveDaysOfWeek.isLeft shouldBe true

    SingleValue(18).resolveMonths.isLeft shouldBe true
    ValuesRange(6, 2).resolveMonths.isLeft shouldBe true
    ValuesRange(21, 37).resolveMonths.isLeft shouldBe true
    Wildcard(Some(0)).resolveMonths.isLeft shouldBe true
    ValuesList(List(1, 4, 18)).resolveMonths.isLeft shouldBe true
  }
}