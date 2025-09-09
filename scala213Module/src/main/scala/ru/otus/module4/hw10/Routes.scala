package ru.otus.module4.hw10


import cats.effect.IO
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.headers.`Content-Type`
import org.http4s.MediaType


class Routes(counterService: CounterService, slowService: SlowService) {
  
  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "counter" =>
      counterService.incrementAndGet.flatMap { json =>
        Ok(json)
      }
    
    case GET -> Root / "slow" / IntVar(chunk) / IntVar(total) / IntVar(time) =>
      if (chunk <= 0 || total <= 0 || time <= 0) {
        BadRequest("All parameters must be positive integers")
      } else {
        slowService.slowResponse(chunk, total, time).flatMap { stream =>
          Ok(stream, `Content-Type`(MediaType.text.plain))
        }
      }
    
    case GET -> Root / "slow" / _ / _ / _ =>
      BadRequest("All parameters must be integers")
  }
}

object Routes {
  def create(counterService: CounterService): IO[Routes] = 
    IO.pure(new Routes(counterService, new SlowService))
}
