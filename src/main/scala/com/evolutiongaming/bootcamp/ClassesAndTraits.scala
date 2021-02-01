package com.evolutiongaming.bootcamp

object ClassesAndTraits {

  sealed trait Shape extends Located with Bounded with Movable {
    def area: Double
  }

  sealed trait Located {
    def x: Double

    def y: Double
  }

  sealed trait Bounded {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Movable
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)

    override def area: Double = 0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double = centerX

    override def y: Double = centerY

    override def minX: Double = centerX - radius

    override def maxX: Double = centerX + radius

    override def minY: Double = centerY - radius

    override def maxY: Double = centerY + radius

    override def move(dx: Double, dy: Double): Circle = Circle(centerX + dx, centerY + dy, radius)

    override def area: Double = math.Pi * math.pow(radius, 2)
  }

  final case class Rectangle(bottomLeftX: Double, bottomLeftY: Double, width: Double, height: Double) extends Shape {
    override def x: Double = bottomLeftX

    override def y: Double = bottomLeftY

    override def minX: Double = bottomLeftX

    override def maxX: Double = bottomLeftX + width

    override def minY: Double = bottomLeftY

    override def maxY: Double = bottomLeftY + height

    override def move(dx: Double, dy: Double): Rectangle = Rectangle(bottomLeftX + x, bottomLeftY + dy, width, height)

    override def area: Double = width * height
  }

  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded =
    new Bounded {
      override def minX: Double = objects.map(_.minX).min

      override def maxX: Double = objects.map(_.maxX).max

      override def minY: Double = objects.map(_.minY).min

      override def maxY: Double = objects.map(_.maxY).max
    }

  final case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape {
    val leftBottom: Point = List(p1, p2, p3).minBy(list => (list.x, list.y))
    val bounded: Bounded = minimumBoundingRectangle(Set(p1, p2, p3))

    override def x: Double = leftBottom.x

    override def y: Double = leftBottom.y

    override def minX: Double = bounded.minX

    override def maxX: Double = bounded.maxX

    override def minY: Double = bounded.minY

    override def maxY: Double = bounded.maxY

    override def move(dx: Double, dy: Double): Movable = Triangle(
      Point(p1.x + dx, p1.y + dy),
      Point(p2.x + dx, p2.y + dy),
      Point(p3.x + dx, p3.y + dy))

    override def area: Double = math.abs(p1.x * (p2.y - p3.y) + p2.x * (p3.y - p1.y) + p3.x * (p1.y - p1.y)) / 2
  }

  final case class Square(bottomLeftX: Double, bottomLeftY: Double, width: Double) extends Shape {
    override def x: Double = bottomLeftX

    override def y: Double = bottomLeftY

    override def minX: Double = bottomLeftX

    override def maxX: Double = bottomLeftX + width

    override def minY: Double = bottomLeftY

    override def maxY: Double = bottomLeftY + width

    override def move(dx: Double, dy: Double): Movable = Square(bottomLeftX + dx, bottomLeftY + dy, width)

    override def area: Double = width * width
  }


  // =======================================================

  sealed trait Located3D {
    def x: Double

    def y: Double

    def z: Double
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Movable3D
  }

  sealed trait Shape3D extends Located3D with Movable3D {
    def surfaceArea: Double

    def volume: Double
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def surfaceArea: Double = 0

    override def volume: Double = 0

    override def move(dx: Double, dy: Double, dz: Double): Movable3D = Point3D(x + dx, y + dy, z + dz)
  }

  final case class Sphere3D(x: Double, y: Double, z: Double, radius: Double) extends Shape3D {
    override def surfaceArea: Double = 4 * math.Pi * math.pow(radius, 2)

    override def volume: Double = 4 / 3 * math.Pi * math.pow(radius, 3)

    override def move(dx: Double, dy: Double, dz: Double): Movable3D = Sphere3D(x + dx, y + dy, z + dz, radius)
  }

  final case class Cube3D(x: Double, y: Double, z: Double, width: Double) extends Shape3D {
    override def surfaceArea: Double = width * width * 6

    override def volume: Double = width * width * width

    override def move(dx: Double, dy: Double, dz: Double): Movable3D = Cube3D(x + dx, y + dy, z + dz, width)
  }

  final case class Cuboid3D(x: Double, y: Double, z: Double, width: Double, length: Double, height: Double) extends Shape3D {
    override def surfaceArea: Double = 2 * width * length + 2 * length * height + 2 * width * height

    override def volume: Double = width * length * height

    override def move(dx: Double, dy: Double, dz: Double): Movable3D = Cuboid3D(x + dx, y + dy, z + dz, width, length, height)
  }

  final case class Triangle3D(p1: Point3D, p2: Point3D, p3: Point3D) extends Shape3D {
    val leftBottom: Point3D = List(p1, p2, p3).minBy(list => (list.x, list.y, list.z))

    override def surfaceArea: Double = ???

    override def volume: Double = 0

    override def move(dx: Double, dy: Double, dz: Double): Movable3D = Triangle3D(
      Point3D(p1.x + dx, p1.y + dy, p1.z + dz),
      Point3D(p2.x + dx, p2.y + dy, p2.z + dz),
      Point3D(p3.x + dx, p3.y + dy, p3.z + dz)
    )

    override def x: Double = leftBottom.x

    override def y: Double = leftBottom.y

    override def z: Double = leftBottom.z
  }
}
