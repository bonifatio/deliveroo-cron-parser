package uk.co.deliveroo

import cats.implicits._
import org.scalatest.EitherValues._
import org.scalatest.{FlatSpec, Matchers}

class CrontabItemSpec extends FlatSpec with Matchers {
  import CrontabItem._

  behavior of "tabular formatting"

  it should "generate proper table" in {
    val crontabItem = CrontabItem(
      minutes = List(15, 45),
      hours = List(12),
      dayOfMonth = List(16, 18, 19),
      month = List(1),
      dayOfWeek = List(2, 4, 6),
      year = None,
      command = "foo"
    )

    val expected =
      """|minute        15 45
         |hour          12
         |day of month  16 18 19
         |month         1
         |day of week   2 4 6
         |command       foo""".stripMargin

    crontabItem.show shouldBe expected
  }

  it should "generate proper table with year" in {
    val crontabItem = CrontabItem(
      minutes = List(15, 45),
      hours = List(12),
      dayOfMonth = List(16, 18, 19),
      month = List(1),
      dayOfWeek = List(2, 4, 6),
      year = Some(List(2020, 2021)),
      command = "foo"
    )

    val expected =
      """|minute        15 45
         |hour          12
         |day of month  16 18 19
         |month         1
         |day of week   2 4 6
         |year          2020 2021
         |command       foo""".stripMargin

    crontabItem.show shouldBe expected
  }

  behavior of "parsing crontab string"

  it should "create proper item from numeric time fragments" in {
    val expected = CrontabItem(
      minutes = List(0, 15, 30, 45),
      hours = List(0),
      dayOfMonth = List(1, 15),
      month = (1 to 12).toList,
      dayOfWeek = (1 to 5).toList,
      year = Some(List(2020)),
      command = "/usr/bin/find"
    )

    parse(" */15 0 1,15 * 1-5 2020 /usr/bin/find ").right.value shouldBe expected
  }

  it should "create proper item from string time fragments" in {
    val expected = CrontabItem(
      minutes = List(0, 15, 30, 45),
      hours = List(0),
      dayOfMonth = List(1, 15),
      month = List(2, 8),
      dayOfWeek = List(1, 2, 3),
      year = None,
      command = "/usr/bin/find"
    )

    parse(" */15 0 1,15 aug,feb mon-wed /usr/bin/find ").right.value shouldBe expected
  }

  it should "handle invalid inputs" in {
    parse(" */15 0 1,35 * 1-5 /usr/bin/find ").isLeft shouldBe true
    parse(" */15 1,3 * 1-5 /usr/bin/find ").isLeft shouldBe true
  }
}