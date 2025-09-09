package ru.otus.module4.hw10


import cats.effect.{Ref, IO}
import io.circe.Json
import io.circe.syntax._


class CounterService(ref: Ref[IO, Long]) {
  def incrementAndGet: IO[Json] = 
    ref.modify(current => (current + 1, current + 1))
      .map(count => Json.obj("counter" -> count.asJson))
}

object CounterService {
  def create: IO[CounterService] = 
    Ref.of[IO, Long](0L).map(new CounterService(_))
}
