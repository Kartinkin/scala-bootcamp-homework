package com.evolutiongaming.bootcamp.implicits

object TypeclassTask {
  trait HashCode[T] {
    def hash(entity: T): Int
  }

  object HashCode {
    def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance
  }

  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit h: HashCode[A]): Int = h.hash(x)
  }

  implicit val StringHash: HashCode[String] =
    str => str.map(_.toInt).fold(0) { (acc, v) => acc + v }
}



object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = Ordering[BigDecimal].on(_.amount)
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  object Show {
    def apply[F](implicit instance: Show[F]): Show[F] = instance
  }

  implicit class ShowSyntax[A](x: A) {
    def show(implicit s: Show[A]): String = s.show(x)
  }

  final case class User(id: Int, name: String)

  implicit val UserShow: Show[User] =
    user => user.name
}

object Task3 {
  type Error = String
  trait Parse[T] {
    def parse(entity: String): Either[Error, T]
  }

  object Parse {
    def apply[F](implicit instance: Parse[F]): Parse[F] = instance
  }

  implicit class ParseSyntax[A](x: String) {
    def parse[A: Parse]: Either[Error, A] = Parse[A].parse(x)
  }

  final case class User(id: String, name: String)

  implicit val UserParse: Parse[User] = entity =>
    entity.split(':').toList match {
      case id :: name :: Nil => Right(User(id, name))
      case default => Left("Error")
    }
}

object Task4 {
  // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`
  trait TypesafeEquals[T] {
    def eq(first: T, second: T): Boolean
  }
  object TypesafeEquals {
    def apply[F: TypesafeEquals]: TypesafeEquals[F] = implicitly
    // def apply[F](implicit instance: TypesafeEquals[F]): TypesafeEquals[F] = instance
  }
  implicit class TypesafeEqualsSyntax[A: TypesafeEquals](first: A) {
    def ===(second: A): Boolean = TypesafeEquals[A].eq(first, second)
  }

  implicit val IntEq: TypesafeEquals[Int] =
    (x, y) => x == y
}

object AdvancedHomework {
  trait TypeclassFlatMap[T[_]] {
    def flatMap[A, B](x: T[A])(f: A => T[B]): T[B]
  }

  implicit val IntEq = new TypeclassFlatMap[Iterable] {
    def flatMap[A, B](x: Iterable[A])(f: A => Iterable[B]): Iterable[B] =
      x.map(f).flatten
  }
}
