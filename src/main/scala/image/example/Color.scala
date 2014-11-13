package image.example

abstract class Color[Repr: Manifest] {
  def r(t: Repr): Float // red
  def g(t: Repr): Float // green
  def b(t: Repr): Float // blue
  def a(t: Repr): Float // alpha
  def pack(r: Float, g: Float, b: Float, a: Float): Repr
  implicit def manifest = implicitly[Manifest[Repr]]
}

object Color {

  /** RGB + alpha */
  class RGBA extends Color[Int] {
    def r(t: Int): Float = (t & 0xFF000000).toFloat / 0xFF000000
    def g(t: Int): Float = (t & 0x00FF0000).toFloat / 0x00FF0000
    def b(t: Int): Float = (t & 0x0000FF00).toFloat / 0x0000FF00
    def a(t: Int): Float = (t & 0x000000FF).toFloat / 0x000000FF
    def pack(r: Float, g: Float, b: Float, a: Float): Int =
      (r * 0xFF000000).toInt & 0xFF000000 +
      (g * 0x00FF0000).toInt & 0x00FF0000 +
      (b * 0x0000FF00).toInt & 0x0000FF00 +
      (a * 0x000000FF).toInt & 0x000000FF
  }
  implicit object RGBA extends RGBA

  case class FullColorEntry[F](r: F, g: F, b: F, a: F)
  class FullColor extends Color[FullColorEntry[Float]] {
    def r(t: FullColorEntry[Float]): Float = t.r
    def g(t: FullColorEntry[Float]): Float = t.g
    def b(t: FullColorEntry[Float]): Float = t.b
    def a(t: FullColorEntry[Float]): Float = t.a
    def pack(r: Float, g: Float, b: Float, a: Float): FullColorEntry[Float] =
      FullColorEntry(r, g, b, a)
  }
  implicit object FullColor extends FullColor

  class RGBAExtended extends Color[Long] {
    def r(t: Long): Float = (t & 0xFFFF000000000000l).toFloat / 0xFFFF000000000000l
    def g(t: Long): Float = (t & 0x0000FFFF00000000l).toFloat / 0x0000FFFF00000000l
    def b(t: Long): Float = (t & 0x00000000FFFF0000l).toFloat / 0x00000000FFFF0000l
    def a(t: Long): Float = (t & 0x000000000000FFFFl).toFloat / 0x000000000000FFFFl
    def pack(r: Float, g: Float, b: Float, a: Float): Long =
      (r * 0xFFFF000000000000l).toLong & 0xFFFF000000000000l +
      (g * 0x0000FFFF00000000l).toLong & 0x0000FFFF00000000l +
      (b * 0x00000000FFFF0000l).toLong & 0x00000000FFFF0000l +
      (a * 0x000000000000FFFFl).toLong & 0x000000000000FFFFl
  }
  implicit object RGBAExtended extends RGBAExtended

  // TODO: Define other color systems
}

