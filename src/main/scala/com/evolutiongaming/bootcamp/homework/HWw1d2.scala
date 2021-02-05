package com.evolutiongaming.bootcamp.basics

object HWw1d2 {
  sealed trait Space {
    def dim: Int
  }
  object Space {
    final case class Space2d() extends Space { override def dim = 2 }
    final case class Space3d() extends Space { override def dim = 3 }
  }

  import Space._
  case class LinVector[A <: Space] private (p: Vector[Double]) {
    def elements: Vector[Double] = p

    def +(that: LinVector[A]): LinVector[A] =
      new LinVector[A](this.elements.zip(that.elements).map { case(_1, _2) => _1 + _2 })

    def -(that: LinVector[A]): LinVector[A] =
      new LinVector[A](this.elements.zip(that.elements).map { case(_1, _2) => _1 - _2 })

    def mod: Double =
      Math.sqrt(this.elements.map { x => x * x }.sum)

    override def equals(that: Any): Boolean = that match {
      case that: LinVector[A] => this.elements == that.elements
      case _ => false
    }
  }

  object LinVector {
    def apply[A <: Space](p: Vector[Double]) = new LinVector[A](p)
    def apply[A <: Space](p: LinVector[A]): LinVector[A] = new LinVector[A](p.elements)
    def apply(x: Double, y: Double) = new LinVector[Space2d](Vector(x, y))
    def apply(x: Double, y: Double, z: Double)  = new LinVector[Space3d](Vector(x, y, z))
  }


  sealed trait Shape[A <: Space] extends Located[A] with Movable[A] with Bounded[A]

  sealed trait Shape2d {
    def area: Double
  }

  sealed trait Shape3d {
    def surfaceArea: Double
    def volume: Double
  }


  sealed trait Located[A <: Space] {
    def coord: LinVector[A]
  }

  sealed trait Movable[A <: Space] {
    def move(delta: LinVector[A]): Located[A]
  }

  sealed trait Bounded[A <: Space] { // with n-dimension cube
    def min: LinVector[A]
    def max: LinVector[A]
  }

  final case class Point[A <: Space](p: LinVector[A]) extends Shape[A] {
    override def coord: LinVector[A] = p
    override def move(delta: LinVector[A]): Point[A] = Point(coord + delta)
    override def min: LinVector[A] = p
    override def max: LinVector[A] = p
  }


  final case class Circle(center: LinVector[Space2d], radius: Double) extends Shape[Space2d] with Shape2d {
      if (radius < 0)
        throw new IllegalArgumentException("Radius must be non-negative.")
    override def coord: LinVector[Space2d] = center
    override def move(delta: LinVector[Space2d]): Circle = Circle(coord + delta, radius)
    override def area: Double = Math.PI * radius * radius
    override def min: LinVector[Space2d] = center - LinVector(radius, radius)
    override def max: LinVector[Space2d] = center + LinVector(radius, radius)
  }

  final case class Square(leftBottom: LinVector[Space2d], side: Double) extends Shape[Space2d] with Shape2d {
      if (side < 0)
        throw new IllegalArgumentException("Cube side must be non-negative.")
    override def coord: LinVector[Space2d] = leftBottom
    override def move(delta: LinVector[Space2d]): Square = Square(leftBottom + delta, side)
    override def area: Double = side * side
    override def min: LinVector[Space2d] = leftBottom
    override def max: LinVector[Space2d] = leftBottom + LinVector(side, side)
  }

  final case class TriangleNd[A <: Space](v1: LinVector[A], v2: LinVector[A], v3: LinVector[A]) extends Shape[A] with Shape2d {
    override def coord: LinVector[A] = v1
    def vertice1: LinVector[A] = v1
    def vertice2: LinVector[A] = v2
    def vertice3: LinVector[A] = v3
    override def move(delta: LinVector[A]): TriangleNd[A] = TriangleNd[A](v1 + delta, v2 + delta, v3 + delta)
    override def area: Double = {
      val a = (v1 - v2).mod
      val b = (v2 - v3).mod
      val c = (v1 - v3).mod
      val p = (a + b + c) / 2
      Math.sqrt(p * (p - a) * (p - b) * (p - c))
    }
    override def min: LinVector[A] = LinVector[A](Vector(v1.elements, v2.elements, v3.elements).transpose.map { _.min })
    override def max: LinVector[A] = LinVector[A](Vector(v1.elements, v2.elements, v3.elements).transpose.map { _.max })
  }

  type Triangle = TriangleNd[Space2d]
  type Triangle3d = TriangleNd[Space3d]

  final case class Cube(leftBottom: LinVector[Space3d], side: Double) extends Shape[Space3d] with Shape3d {
      if (side < 0)
        throw new IllegalArgumentException("Cube side must be non-negative.")
    override def coord: LinVector[Space3d] = leftBottom
    override def move(delta: LinVector[Space3d]): Cube = Cube(leftBottom + delta, side)
    override def volume: Double = side * side * side
    override def surfaceArea: Double = 6 * side * side
    override def min: LinVector[Space3d] = leftBottom
    override def max: LinVector[Space3d] = leftBottom + LinVector(side, side, side)
  }

  val Origin2d = LinVector(0, 0)
  val Origin3d = LinVector(0, 0, 0)

}
