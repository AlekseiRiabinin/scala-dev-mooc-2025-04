package ru.otus.module2

// Using enum for sealed trait hierarchy (new Scala 3 way)
enum JsValue:
  case JsObject(get: Map[String, JsValue])
  case JsString(get: String)
  case JsNumber(get: Double)
  case JsNull

object JsValue:
  // Extension methods for convenience
  extension (js: JsValue)
    def prettyPrint: String = js match
      case JsObject(fields) => 
        fields.map { case (k, v) => s""""$k": ${v.prettyPrint}""" }.mkString("{ ", ", ", " }")
      case JsString(s) => s""""$s""""
      case JsNumber(n) => n.toString
      case JsNull => "null"

// Using typeclass approach with given/using syntax
trait JsonWriter[T]:
  def write(v: T): JsValue

object JsonWriter:
  // Summoner method
  def apply[T](using ev: JsonWriter[T]): JsonWriter[T] = ev
  
  // Constructor using context functions
  def from[T](f: T => JsValue): JsonWriter[T] =
    new JsonWriter[T]:
      override def write(v: T): JsValue = f(v)
  
  // Given instances (replacing implicit vals)
  given intJsonWriter: JsonWriter[Int] = from(n => JsValue.JsNumber(n.toDouble))
  given stringJsonWriter: JsonWriter[String] = from(JsValue.JsString(_))
  
  // Given instance with context bound (implicit parameter)
  given [T](using ev: JsonWriter[T]): JsonWriter[Option[T]] with
    def write(v: Option[T]): JsValue = v match
      case Some(value) => ev.write(value)
      case None => JsValue.JsNull
  
  // Extension method for any type that has a JsonWriter
  extension [T](value: T)(using writer: JsonWriter[T])
    def toJson: JsValue = writer.write(value)
