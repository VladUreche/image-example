package image.example

import language.implicitConversions

abstract class Pixel[Repr: Manifest] {
  def r(t: Repr): Double // red
  def g(t: Repr): Double // green
  def b(t: Repr): Double // blue
  def a(t: Repr): Double // alpha
  def pack(r: Double, g: Double, b: Double, a: Double): Repr
  // to allow Image to build an array:
  implicit def manifest = implicitly[Manifest[Repr]]
}

/** Different implementations of the Pixel system */
object Pixel {

  /** 8-bit RGB channels + 8-bit alpha, encoded in integers */
  class RGBA extends Pixel[Int] {
    def r(t: Int): Double = (t & 0xFF000000).toDouble / 0xFF000000
    def g(t: Int): Double = (t & 0x00FF0000).toDouble / 0x00FF0000
    def b(t: Int): Double = (t & 0x0000FF00).toDouble / 0x0000FF00
    def a(t: Int): Double = (t & 0x000000FF).toDouble / 0x000000FF
    def pack(r: Double, g: Double, b: Double, a: Double): Int =
      (r * 0xFF000000).toInt & 0xFF000000 +
      (g * 0x00FF0000).toInt & 0x00FF0000 +
      (b * 0x0000FF00).toInt & 0x0000FF00 +
      (a * 0x000000FF).toInt & 0x000000FF
  }
  implicit object RGBA extends RGBA

  /** Abstract 4-channel Pixel, parameterized on the channel representation */
  abstract class FourChannelPixel[T] extends Pixel[FourChannelEntry[T]]()(manifest[AnyRef].asInstanceOf[Manifest[FourChannelEntry[T]]]) {
    // for the constructor argument, see https://github.com/miniboxing/miniboxing-plugin/issues/144
    implicit def toDouble(t: T): Double
    implicit def fromDouble(t: Double): T
    def r(t: FourChannelEntry[T]): Double = t.r
    def g(t: FourChannelEntry[T]): Double = t.g
    def b(t: FourChannelEntry[T]): Double = t.b
    def a(t: FourChannelEntry[T]): Double = t.a
    def pack(r: Double, g: Double, b: Double, a: Double): FourChannelEntry[T] =
      FourChannelEntry(r, g, b, a)
  }

  /** 4-channel entry, parameterized on the channel representation */
  case class FourChannelEntry[T](r: T, g: T, b: T, a: T)

  /** Storing values as doubles, to enable transformations */
  implicit object FullPixel extends FourChannelPixel[Double] {
    implicit def toDouble(t: Double): Double = t
    implicit def fromDouble(t: Double): Double = t
  }

  /** Storing values as floates, to enable transformations while conserving space */
  implicit object HalfPixel extends FourChannelPixel[Float] {
    implicit def toDouble(t: Float): Double = t
    implicit def fromDouble(t: Double): Float = t.toFloat
  }

  /** Storing 16-bit colors instead of 8-bit */
  class RGBAExtended extends Pixel[Long] {
    def r(t: Long): Double = (t & 0xFFFF000000000000l).toDouble / 0xFFFF000000000000l
    def g(t: Long): Double = (t & 0x0000FFFF00000000l).toDouble / 0x0000FFFF00000000l
    def b(t: Long): Double = (t & 0x00000000FFFF0000l).toDouble / 0x00000000FFFF0000l
    def a(t: Long): Double = (t & 0x000000000000FFFFl).toDouble / 0x000000000000FFFFl
    def pack(r: Double, g: Double, b: Double, a: Double): Long =
      (r * 0xFFFF000000000000l).toLong & 0xFFFF000000000000l +
      (g * 0x0000FFFF00000000l).toLong & 0x0000FFFF00000000l +
      (b * 0x00000000FFFF0000l).toLong & 0x00000000FFFF0000l +
      (a * 0x000000000000FFFFl).toLong & 0x000000000000FFFFl
  }
  implicit object RGBAExtended extends RGBAExtended

  // TODO: Define other Pixel representations
}

