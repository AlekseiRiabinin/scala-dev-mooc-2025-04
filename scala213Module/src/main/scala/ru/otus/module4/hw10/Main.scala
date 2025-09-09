package ru.otus.module4.hw10


import cats.effect.{IO, IOApp}
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s._


object Main extends IOApp.Simple {
  
  def run: IO[Unit] = 
    for {
      counterService <- CounterService.create
      routes <- Routes.create(counterService)
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(routes.routes.orNotFound)
        .build
        .use(_ => IO.never)
    } yield ()
}
