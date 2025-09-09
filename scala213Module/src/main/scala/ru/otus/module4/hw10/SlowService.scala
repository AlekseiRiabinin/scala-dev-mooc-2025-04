package ru.otus.module4.hw10


import cats.effect.IO
import fs2.Stream
import scala.concurrent.duration._


class SlowService {
  def slowResponse(chunkSize: Int, totalBytes: Int, delaySeconds: Int): IO[Stream[IO, Byte]] = 
    IO.pure {
      if (chunkSize <= 0 || totalBytes <= 0 || delaySeconds <= 0) {
        Stream.empty
      } else {
        val content = "A"
        val chunks = Math.ceil(totalBytes.toDouble / chunkSize).toInt
        val actualChunkSize = if (chunkSize > totalBytes) totalBytes else chunkSize
        
        Stream
          .range(0, chunks)
          .flatMap { index =>
            val bytesInChunk = 
              if (index == chunks - 1) totalBytes - (index * actualChunkSize)
              else actualChunkSize
              
            val chunkContent = content * bytesInChunk
            val bytes = chunkContent.getBytes("UTF-8")
            
            Stream.emits(bytes) ++ 
              Stream.exec(IO.sleep(delaySeconds.seconds))
          }
      }
    }
}
