package ru.otus.module2

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}


  // General Monad typeclass (hw5)
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  }

  // General tupleF implementation using Monad (hw5)
  def tupleF[F[_], A, B](fa: F[A], fb: F[B])(implicit m: Monad[F]): F[(A, B)] =
    m.flatMap(fa)(a => m.map(fb)(b => (a, b)))

  // Monad instances for common types (hw5)
  object MonadInstances {
    implicit val optionMonad: Monad[Option] = new Monad[Option] {
      def pure[A](a: A): Option[A] = Some(a)
      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    implicit val listMonad: Monad[List] = new Monad[List] {
      def pure[A](a: A): List[A] = List(a)
      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }

    implicit def eitherMonad[E]: Monad[({ type L[A] = Either[E, A] })#L] = 
      new Monad[({ type L[A] = Either[E, A] })#L] {
        def pure[A](a: A): Either[E, A] = Right(a)
        def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = 
          fa.flatMap(f)
      }
  }


  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }


  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](f: A => B): Option[B] = opt.map(f)

    override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }

  def listBindable[A](opt: List[A]): Bindable[List, A] = new Bindable[List, A] {
    override def map[B](f: A => B): List[B] = opt.map(f)

    override def flatMap[B](f: A => List[B]): List[B] = opt.flatMap(f)
  }



  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r1 = println(tupleBindable(optBindable(optA), optBindable(optB)))
  val r2 = println(tupleBindable(listBindable(list1), listBindable(list2)))



}