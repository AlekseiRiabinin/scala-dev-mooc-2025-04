package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp, Ref}
import cats.effect.kernel.Fiber
import cats.implicits._
import scala.concurrent.duration._

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())

object WalletFibersApp extends IOApp.Simple {

  def backgroundTopup(wallet: Wallet[IO], interval: FiniteDuration): IO[Fiber[IO, Throwable, Unit]] = {
    def loop: IO[Unit] = 
      wallet.topup(100) >> IO.sleep(interval) >> loop
    
    loop.start
  }

  def backgroundPrintBalances(
    wallet1: Wallet[IO], 
    wallet2: Wallet[IO], 
    wallet3: Wallet[IO]
  ): IO[Fiber[IO, Throwable, Unit]] = {
    def loop: IO[Unit] = 
      for {
        b1 <- wallet1.balance
        b2 <- wallet2.balance
        b3 <- wallet3.balance
        _ <- IO.println(s"Balances: $b1, $b2, $b3")
        _ <- IO.sleep(1.second)
        _ <- loop
      } yield ()
    
    loop.start
  }

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      
      // Start background processes
      fiber1 <- backgroundTopup(wallet1, 100.millis)
      fiber2 <- backgroundTopup(wallet2, 500.millis)
      fiber3 <- backgroundTopup(wallet3, 2000.millis)
      printFiber <- backgroundPrintBalances(wallet1, wallet2, wallet3)
      
      // Wait for user input
      _ <- IO.readLine
      
      // Cancel all fibers
      _ <- fiber1.cancel
      _ <- fiber2.cancel
      _ <- fiber3.cancel
      _ <- printFiber.cancel
    } yield ()
}
