package image.example

import java.io.File

/** An abstract image */
abstract class Image[Repr: Color] {
  def width: Int
  def height: Int
  def apply(x: Int, y: Int): Repr
  def map[Repr2: Color](f: Image[Repr] => Generator[Repr2]): Image[Repr2]
}

object Image {
  /** Create an empty image */
  def empty[Repr: Color](width: Int, height: Int): Image[Repr] =
    new ImageImpl[Repr](width, height)

  // TODO: Load, save, etc...
}

/** Image implementation */
private class ImageImpl[Repr: Color](val width: Int, val height: Int) extends Image[Repr] {
  // internal storage
  val array = implicitly[Color[Repr]].manifest.newArray(width * height)

  // get pixel, public
  def apply(x: Int, y: Int): Repr = {
    assert(x < width)
    assert(y < height)
    array(x + y * width)
  }

  // set pixel, protected
  protected def update(x: Int, y: Int, t: Repr): Unit = {
    assert(x < width)
    assert(y < height)
    array(x + y * width) = t
  }

  def map[Repr2: Color](f: Image[Repr] => Generator[Repr2]): Image[Repr2] = {
    val gen = f(this)
    new ImageImpl[Repr2](gen.width, gen.height) {
      var x = 0
      var y = 0
      while (y < height) {
        while (x < width) {
          this(x, y) = gen.generate(x, y)
          x += 1
        }
        x = 0
        y += 1
      }
    }
  }
}
