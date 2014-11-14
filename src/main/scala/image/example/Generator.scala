package image.example

abstract class Generator[Repr: Color] {
  def width: Int
  def height: Int
  def generate(x: Int, y: Int): Repr
}

object Generator {

  def convertTo[Repr, Repr2](to: Color[Repr2])(implicit from: Color[Repr]) =
    (image: Image[Repr]) => new Generator[Repr2]()(to) {
      def width = image.width
      def height = image.height
      def generate(x: Int, y: Int): Repr2 = {
        val pixel = image(x, y)
        val r = from.r(pixel)
        val g = from.g(pixel)
        val b = from.b(pixel)
        val a = from.a(pixel)

        to.pack(r, g, b, a)
      }
    }

  def scale[Repr: Color](scale: Float) =
    (image: Image[Repr]) => new Generator[Repr] {
      def width = (image.width * scale).toInt
      def height = (image.height * scale).toInt
      def generate(x: Int, y: Int): Repr = {
        val encoding = implicitly[Color[Repr]]
        image((x.toFloat / scale).toInt, (y.toFloat / scale).toInt)
      }
    }

  def invert[Repr: Color] =
    (image: Image[Repr]) => new Generator[Repr] {
      def width = image.width
      def height = image.height
      def generate(x: Int, y: Int): Repr = {
        val encoding = implicitly[Color[Repr]]
        val pixel = image(x, y)
        val r = encoding.r(pixel)
        val g = encoding.g(pixel)
        val b = encoding.b(pixel)
        val a = encoding.a(pixel)

        encoding.pack(1-r, 1-g, 1-b, a)
      }
    }

  /** Generator that blurs an existing image */
  def blur[Repr: Color](size: Int) =
    (image: Image[Repr]) => new Generator[Repr] {
      def width = image.width
      def height = image.height
      def generate(x: Int, y: Int): Repr = {
        val w = image.width
        val h = image.height

        var xd = -size + 1
        var yd = -size + 1

        var r: Double = 0
        var g: Double = 0
        var b: Double = 0
        var a: Double = 0
        var tot: Int = 0

        val encoding = implicitly[Color[Repr]]

        while (xd < size) {
          while (yd < size) {
            val nx = x + xd
            val ny = y + yd
            if ((nx >= 0) && (nx < w) && (ny >= 0) && (ny < h)) {
              val c = image(nx, ny)
              r += encoding.r(c)
              g += encoding.g(c)
              b += encoding.b(c)
              a += encoding.a(c)
              tot += 1
            }
            yd += 1
          }
          yd = -size + 1
          xd += 1
        }

        encoding.pack(r/tot, g/tot, b/tot, a/tot)
      }
  }
}