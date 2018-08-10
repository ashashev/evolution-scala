package evolution

import annotation._

object Area {
  def apply(width: Int, height: Int): Area =
    apply((width, height))
  def apply(size: (Int, Int)): Area =
    new Area(size, Vector.fill[Option[Cell]](size._2, size._1)(None))
}

final class Area(
  val size:          (Int, Int),
  private val impl_ :Vector[Vector[Option[Cell]]]) {

  assert(size._1 > 0 && size._2 > 0)

  def get(p: (Int, Int)): Option[Cell] =
    impl_(p._2)(p._1)

  def apply(p: (Int, Int)): Cell =
    get(p) match {
      case None    => throw new RuntimeException(s"The $p position is empty")
      case Some(c) => c
    }

  def +(e: ((Int, Int), Cell)): Area =
    updated(e._1, e._2)

  def -(p: (Int, Int)): Area =
    updated_(p, None)

  def ++(that: Area): Area = {
    def copy(z: Area, pn: ((Int, Int), Cell)) = { val (p, n) = pn; z.updated(p, n) }

    val newSize = (size._1 max that.size._1, size._2 max that.size._2)

    val acc =
      if (newSize == size) this
      else foldLeft(Area(newSize))(copy)

    that.foldLeft(acc)(copy)
  }

  def updated(p: (Int, Int), n: Cell): Area =
    updated_(p, Some(n))

  def map[T <: Cell](f: ((Int, Int), Cell) => T): Area = {
    foldLeft(Area(size))((z, pn) => { val (p, n) = pn; z.updated(p, f(p, n)) })
  }

  def flatMap(f: ((Int, Int), Cell) => Area): Area = {
    def op(z: Area, pn: ((Int, Int), Cell)) = { val (p, n) = pn; z ++ f(p, n) }
    foldLeft(Area(size))(op)
  }

  def foreach(f: ((Int, Int), Cell) => Unit): Unit = {
    for {
      y <- 0 until size._2
      x <- 0 until size._1
      cell <- impl_(y)(x)
    } {
      f((x, y), cell)
    }
  }

  def /:[B](z: B)(op: (B, ((Int, Int), Cell)) => B) =
    foldLeft(z)(op)

  def foldLeft[B](z: B)(op: (B, ((Int, Int), Cell)) => B): B = {
    @tailrec
    def traverse(p: (Int, Int), acc: B): B =
      p match {
        case (_, size._2) => acc
        case (size._1, y) => traverse((0, y + 1), acc)
        case p @ (x, y) => get(p) match {
          case None    => traverse((x + 1, y), acc)
          case Some(n) => traverse((x + 1, y), op(acc, (p, n)))
        }
      }

    traverse((0, 0), z)
  }

  def :\[B](z: B)(op: (((Int, Int), Cell), B) => B): B =
    foldRight(z)(op)

  def foldRight[B](z: B)(op: (((Int, Int), Cell), B) => B): B = {
    @tailrec
    def traverse(p: (Int, Int), acc: B): B =
      p match {
        case (_, -1) => acc
        case (-1, y) => traverse((size._1 - 1, y - 1), acc)
        case p @ (x, y) => get(p) match {
          case None    => traverse((x - 1, y), acc)
          case Some(n) => traverse((x - 1, y), op((p, n), acc))
        }
      }

    traverse((size._1 - 1, size._2 - 1), z)
  }

  override def equals(that: Any): Boolean = that match {
    case that: Area => that.size == size && that.impl_ == impl_
    case _ => false
  }

  override def hashCode(): Int = (size, impl_).##

  private def updated_(p: (Int, Int), n: Option[Cell]): Area =
    new Area(size, impl_.updated(p._2, impl_(p._2).updated(p._1, n)))
}
