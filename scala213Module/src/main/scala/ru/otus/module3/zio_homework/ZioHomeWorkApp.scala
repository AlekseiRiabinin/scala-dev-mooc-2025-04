package ru.otus.module3.zio_homework

import zio._
import java.io.IOException
import zio.Console
import zio.Clock

object ZioHomeWorkApp extends App {
  def run(args: List[String]): URIO[Clock with Console, ExitCode] = {
    runApp
      .as(ExitCode.success)
      .catchAll { e =>
        Console.printLine(s"Application failed: ${e.getMessage}")
          .catchAll(_ => ZIO.unit)
          .as(ExitCode.failure)
      }
  }
}
