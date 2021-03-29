package com.evolutiongaming.bootcamp.cats

import cats.Functor
import cats.data.NonEmptyList
import cats.kernel.Monoid
import cats.implicits._

  /*
    Functor[F[A]] {
      def map(f: A => B): F[B]
    }
   */

  /*
    Applicative[F[A]] {
      def ap(f: F[A => B]): F[B]
    }

    def pure[A](x: A): F[A]
   */

  /*
    Monad[F[A]] {
      def flatMap(f: A => F[B]): F[B]
    }

    def pure[A](x: A): F[A]
   */

object Excercises {
  trait Applicative[F[_]] extends Functor[F] {

    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
    def unit[A](a: => A): F[A]

    // implement methods using each other
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)

    def apply1[A,B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fa, fab){ (a, f) => f(a) }

    def sequence[A](fas: List[F[A]]): F[List[A]] =
      fas.foldRight(unit(List.empty[A])) {
        (el, l) => map2(el, l) { (fel, fl) => fel :: fl }
      }

    def sequence1[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas) { fa => fa }

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B])) {
        (el, l) => map2(f(el), l) { (fel, fl) => fel :: fl }
      }

    def traverse1[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      sequence(as.map(f))

  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

    // implement methods using each other
    def flatMap1[A,B](ma: M[A])(f: A => M[B]): M[B] =
      join(map(ma)(f))

    def join[A](mma: M[M[A]]): M[A] =
      flatMap(mma) { ma => ma }

    def map[A,B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma) { el => unit(f(el)) }

    def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma) { a =>
        map(mb) { b => f(a, b) }
      }
  }
}

object Monads {
  import scala.language.higherKinds

  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def bind[A, B](ma: M[A])(amb: A => M[B]): M[B]
  }

  trait Monoid[A] {
    def mempty: A
    def mappend(x: A)(y: A): A
  }


  case class Identity[A](a: A)
  object Identity {
    implicit val identityMonad = new Monad[Identity] {
      def unit[A](a: A): Identity[A] = new Identity(a)
      def bind[A, B](ma: Identity[A])(amb: A => Identity[B]): Identity[B] = amb(ma.a)
    }
  }

  sealed trait Maybe[+A]
  case class Just[A](a: A) extends Maybe[A]
  case object None extends Maybe[Nothing]
  object Maybe {
    implicit val maybeMonad = new Monad[Maybe] {
      def unit[A](a: A): Maybe[A] = new Just(a)
      def bind[A, B](ma: Maybe[A])(amb: A => Maybe[B]): Maybe[B] = ma match {
        case None => None
        case Just(a) => amb(a)
      }
    }
  }

  case class State[S, A](run: S => (S, A))
  object State {
    // inspired by https://stackoverflow.com/a/6248296
    implicit def stateMonad[S] = new Monad[({type x[a]=State[S, a]})#x] {
      def unit[A](a: A): State[S, A] =
        new State(s => (s, a))
      def bind[A, B](ma: State[S, A])(amb: A => State[S, B]): State[S, B] =
        new State(x => ma.run(x) match {
          case (s, a) => amb(a).run(s) }
        )
    }
  }

  case class Reader[R, A](run: R => A)
  object Reader {
    implicit def readerMonad[R] = new Monad[({type x[a]=Reader[R, a]})#x] {
      def unit[A](a: A): Reader[R, A] = new Reader(_ => a)
      def bind[A, B](ma: Reader[R, A])(amb: A => Reader[R, B]): Reader[R, B] =
        new Reader(r => amb(ma.run(r)).run(r))
    }
  }

  case class Writer[W, A](run: (W, A))
  object Writer {
    implicit def readerMonad[W](implicit m: Monoid[W]) = new Monad[({type x[a]=Writer[W, a]})#x] {
      def unit[A](a: A): Writer[W, A] = new Writer((m.mempty, a))
      def bind[A, B](ma: Writer[W, A])(amb: A => Writer[W, B]): Writer[W, B] =
        ma.run match {
          case (aw, a) => amb(a).run match {
            case (bw, b) => new Writer((m.mappend(aw)(bw), b))
          }
        }
    }
  }
}
