package uk.co.deliveroo

import cats.implicits._

object CrontabParserApp extends App {

  private def usage = println(
    """
      |Usage: java -jar cronparse.jar "*/15 0 1,15 * 1-5 2020 /usr/bin/find"
      |""".stripMargin
  )

  if (args.length != 1) {
    usage
  } else {
    val cronLine = args(0)

    val result = CrontabItem.parse(cronLine).fold(
      err => s"Cannot parse line: $err",
      crontabItem => crontabItem.show
    )

    println(result)
  }

}