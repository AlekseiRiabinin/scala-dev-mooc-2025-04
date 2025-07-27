package ru.otus.module3

import scala.language.postfixOps
import java.io.IOException
import zio._
import zio.Console._
import zio.Random._
import zio.Clock
import java.util.concurrent.TimeUnit
import ru.otus.module3.zio_homework.config.{AppConfig, Configuration}


package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  lazy val guessProgram = {
    val randomNumber = nextIntBetween(1, 4)
    
    for {
      num    <- randomNumber
      _      <- printLine("Guess a number between 1 and 3:")
      input  <- readLine
      _      <- if (input.trim.toIntOption.contains(num)) 
                  printLine("You guessed right!") 
                else 
                  printLine(s"You guessed wrong! The number was $num")
    } yield ()
  }

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](effect: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] = {
    effect.flatMap { a =>
      if (condition(a)) ZIO.succeed(a)
      else doWhile(effect)(condition)
    }
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "Configuration.config" из пакета config
   */

  def loadConfigOrDefault = {
    Configuration.config.catchAll { error =>
      val default = AppConfig("localhost", "8080")
      for {
        _ <- printLine(s"Error loading config: $error. Using default config: $default")
      } yield default
    }
  }

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  
  lazy val eff = nextIntBetween(0, 11).delay(1.second)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  
  lazy val effects = List.fill(10)(eff)
  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, (A, Long)] = {
    for {
      start <- Clock.currentTime(TimeUnit.MILLISECONDS)
      res   <- zio
      end   <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _     <- printLine(s"Execution time: ${end - start} ms").orDie
    } yield (res, end - start)
  }

  lazy val app = {
    for {
      results <- ZIO.collectAll(effects)
      sum = results.sum
      _ <- printLine(s"Sum: $sum")
    } yield sum
  }

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = {
    for {
      fiberList <- ZIO.foreach(effects)(_.fork)
      results   <- ZIO.foreach(fiberList)(_.join)
      sum = results.sum
      _ <- printLine(s"Sum: $sum")
    } yield sum
  }

  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine например
   */

  object TimingService {
    def logTiming[R <: Clock, E, A](label: String)(zio: ZIO[R, E, A]): ZIO[R with Console, E, A] = {
      for {
        start <- Clock.currentTime(TimeUnit.MILLISECONDS)
        res   <- zio
        end   <- Clock.currentTime(TimeUnit.MILLISECONDS)
        _     <- Console.printLine(s"$label took ${end - start} ms").orDie
      } yield res
    }
  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = TimingService.logTiming("Sum calculation")(app)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: ZIO[Clock with Console, IOException, Int] = 
    appWithTimeLogg

}
