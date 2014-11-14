package image.example

import Generator._

object Test {
  def main(args: Array[String]): Unit = {
    val image = Image.empty(500, 500)(Pixel.RGBA)
    for (a <- 1 to 5)
      timed {
        image.
          map(convertTo(Pixel.FullPixel)).
          map(invert).
          map(scale(2)).
          map(blur(10)).
          map(convertTo(Pixel.RGBA))
      }
  }

  def timed[T](t: => T): T = {
    val start = System.currentTimeMillis()
    val res: T = t
    val stop = System.currentTimeMillis()
    println("Operation took " + (stop-start) + " milliseconds.")
    res
  }
}
