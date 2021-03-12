package com.evolutiongaming.bootcamp.typeclass.v2

object QAndAExamples {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[T](implicit instance: Semigroup[T]): Semigroup[T] = instance
  }

  implicit class SemigroupOps[A: Semigroup](x: A) {
    def combine(y: A): A =  Semigroup[A].combine(x, y)
  }

  // 1.2. Implement Semigroup for Long, String
  // implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => x + y
  // implicit val stringSemigroup: Semigroup[String] = (x: String, y: String) => x + y

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists
  def combineAll[A: Semigroup](l: List[A]): A =
    l.reduce { (x, y) => x.combine(y) }


  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists
  def combineAll[A: Semigroup](l: List[A], startingElement: A): A =
    l.foldLeft(startingElement) { (x, y) => x.combine(y) }

  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup
  trait Monoid[A] extends Semigroup[A] {
    def empty(): A
  }

  // 2.2. Implement Monoid for Long, String
  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    override def combine(x: Int, y: Int): Int = x + y
    override def empty(): Int = 0
  }
  implicit val longMonoid: Monoid[Long] = new Monoid[Long] {
    override def combine(x: Long, y: Long): Long = x + y
    override def empty(): Long = 0
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def combine(x: String, y: String): String = x + y
    override def empty(): String = ""
  }

  // 2.3. Implement combineAll(list: List[A]) for all lists
  def assocAll[A: Monoid](l: List[A]): A =
    l.foldLeft(implicitly[Monoid[A]].empty()) { (x, y) => x.combine(y) }

  // 2.4. Implement Monoid for Option[A]
  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(xs), Some(ys)) => Some(xs.combine(ys))
      case (None, None) => None
      case (_, None) => x
      case (None, _) => y
    }
    override def empty(): Option[A] = None
  }

  // 2.5. Implement Monoid for Function1 (for result of the function)
  implicit def function1Monoid[A, B: Monoid]: Monoid[A => B] = new Monoid[A => B] {
    override def combine(f: A => B, g: A => B): A => B =
      x => implicitly[Monoid[B]].combine(f(x), g(x))
    override def empty(): A => B =
      x => implicitly[Monoid[B]].empty()
  }

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  // 4.2. Implement Semigroupal for Option
  implicit def optionSemigroupal: Semigroupal[Option] = new Semigroupal[Option] {
      override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
        case (Some(a), Some(b)) => Some((a, b))
        case default => None
      }
  }


  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]

  implicit class SemigroupalMapNTuple[A, B, F[_]: Semigroupal : Functor](x: Tuple2[F[A], F[B]]) {
    def mapN[R](f: (A, B) => R): F[R] =
      implicitly[Semigroupal[F]].product(x._1, x._2).fmap { case (a, b) => f(a, b) }
  }

  // 4.4. Implement Semigroupal for Map
  implicit def mapSemigroupal[K]: Semigroupal[Map[K, *]] = new Semigroupal[Map[K, *]] {
    override def product[A, B](fa: Map[K, A], fb: Map[K, B]): Map[K, (A, B)] =
      (for { k <- fa.keySet.intersect(fb.keySet) }
        yield (k, (fa(k), fb(k)))
      ).toMap
  }

  implicit def mapFunctor[K] = new Functor[Map[K, *]] {
    override def fmap[A, B](fa: Map[K, A])(f: A => B): Map[K, B] =
      fa.view.mapValues(v => f(v)).toMap
  }

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  // 5.1. Implement Applicative for Option, Either
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(a), Some(b)) => Some((a, b))
      case default => None
    }
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def eitherApplicative: Applicative[Either[String, *]] = new Applicative[Either[String, *]] {
    override def pure[A](a: A): Either[String, A] = Right(a)
    override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] =
      (fa, fb) match {
        case (Right(a), Right(b)) => Right((a, b))
        case (Left(a), _)         => Left(a)
        case (_, Left(b))         => Left(b)
      }
    override def fmap[A, B](fa: Either[String, A])(f: A => B): Either[String, B] =
      fa.map(r => f(r))
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B, F[_]: Applicative](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(implicitly[Applicative[F]].pure(List.empty[B])) {
        (el, acc) => (acc, f(el)).mapN { (xs, x) => x :: xs }
      }

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`
  trait Foldable[F[_]] {
    def empty[A](): F[A]
    def foldLeftN[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  }

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library
  implicit def listFoldable: Foldable[List] = new Foldable[List] {
    override def empty[A](): List[A] = List[A]()
    override def foldLeftN[A, B](fa: List[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b){ (acc, a) => f(acc, a) }
  }

  // 6.3. Implement `traverse` for all Foldables instead of List
  def traverseFoldable[A, B: Semigroup, F[_]: Applicative, G[_]: Foldable](ga: G[A])(f: A => F[B]): F[G[B]] =
    implicitly[Foldable[G]]
    .foldLeftN(ga, implicitly[Applicative[F]].pure[G[B]](implicitly[Foldable[G]].empty())) {
        (acc, el) => (acc, f(el)).mapN { ??? }
      }

}
