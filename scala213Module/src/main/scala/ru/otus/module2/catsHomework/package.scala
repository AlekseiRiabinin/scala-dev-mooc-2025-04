package ru.otus.module2

import cats.Functor
import cats.MonadError
import scala.util.{Try, Success, Failure}

package object catsHomework {

  /**
   * Простое бинарное дерево
   * @tparam A
   */
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  lazy val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  /**
   * Monad абстракция для последовательной
   * комбинации вычислений в контексте F
   * @tparam F
   */
  trait Monad[F[_]]{
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](v: A): F[A]
  }

  /**
   * MonadError расширяет возможность Monad
   * кроме последовательного применения функций, позволяет обрабатывать ошибки
   * @tparam F
   * @tparam E
   */
  trait MonadError[F[_], E] extends Monad[F]{
    def raiseError[A](e: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
    def handleError[A](fa: F[A])(f: E => A): F[A]
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  lazy val tryME: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {
    def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
    def pure[A](v: A): Try[A] = Success(v)
    
    def raiseError[A](e: Throwable): Try[A] = Failure(e)
    def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = fa.recoverWith { case e => f(e) }
    def handleError[A](fa: Try[A])(f: Throwable => A): Try[A] = fa.recover { case e => f(e) }
    def ensure[A](fa: Try[A])(e: Throwable)(f: A => Boolean): Try[A] = 
      fa.flatMap(a => if (f(a)) Success(a) else Failure(e))
  }

  type EitherString[A] = Either[String, A]

  val eitherME: MonadError[EitherString, String] = new MonadError[EitherString, String] {
    def flatMap[A, B](fa: EitherString[A])(f: A => EitherString[B]): EitherString[B] = fa.flatMap(f)
    def pure[A](v: A): EitherString[A] = Right(v)
    
    def raiseError[A](e: String): EitherString[A] = Left(e)
    def handleErrorWith[A](fa: EitherString[A])(f: String => EitherString[A]): EitherString[A] = 
      fa.left.flatMap(f)

  def handleError[A](fa: EitherString[A])(f: String => A): EitherString[A] = 
    fa match {
      case Right(a) => Right(a)
      case Left(e) => Right(f(e))
    }
    def ensure[A](fa: EitherString[A])(e: String)(f: A => Boolean): EitherString[A] = 
      fa.flatMap(a => if (f(a)) Right(a) else Left(e))
  }
}
