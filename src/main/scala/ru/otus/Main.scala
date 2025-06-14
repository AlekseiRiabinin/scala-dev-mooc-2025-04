package ru.otus

import ru.otus.module1.{concurrency, executors, future}
import ru.otus.module1.concurrency.{getRatesLocation1, getRatesLocation2, printRunningTime}
import ru.otus.module2.{implicits, type_classes}

import scala.util.{Failure, Success}


object App {
  def main(args: Array[String]): Unit = {

    println(s"Hello from ${Thread.currentThread().getName}")
    println(type_classes.result)
  }
}