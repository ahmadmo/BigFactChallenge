import java.util.concurrent.{ForkJoinPool, RecursiveTask}

import scala.concurrent.duration.DurationDouble

/**
  * @author ahmad
  */
object BigFact {

  def main(args: Array[String]): Unit = {

    println("JIT Warm up ^_^")
    for (_ <- 1 to 20) {
      100000 !
    }

    val start = System.nanoTime

    val (a, b) = (3000000000000L, 10000000L)

    val x = numOfFactDigits(a)
    println(s"numberOfDigits($a!) = $x")
    val xLen = x.toString.length

    val y = {

      println(s"$b! is being calculated...")
      val result = b !

      // Stirling Approximation is a faster way to compute b!,
      // but in this case it's not accurate enough ;)
      // val result = stirlingApproximation(b.toInt)

      // GammaLn is faster too,
      // but similar to Stirling Approximation, it's not accurate enough.
      // val ln = GammaLn.compute(b + 1)
      // val result: BigInt = BigDecimal(Math.E).pow(BigDecimal(ln)).toBigInteger

      // we're sure that numOfDigits(b!) is lower than Int.MaxValue !!!
      val len = numOfFactDigits(b).intValue
      result / BigInt(10).pow(len - xLen)
    }
    println(s"selectDigits($b!, 0, $xLen) = $y")

    val z = (x + y).toString.toList.map(_.asDigit).sum
    println(s"sumOfDigits($x + $y) = $z")

    val elapsed = System.nanoTime
    println(f"time = ${(elapsed - start).nanos.toMillis}%,d ms")
  }

  implicit class LongOps(val n: Long) extends AnyVal {
    def ! : BigInt = compute(n)
  }

  def compute(n: Long): BigInt =
    if (n < 0) throw new IllegalArgumentException(n + " < 0")
    else if (n < 2) 1
    else {
      val fjp = new ForkJoinPool(Runtime.getRuntime.availableProcessors() * 2)
      val res = fjp.invoke(new RangeProduct(1, n))
      fjp.shutdown()
      res
    }

  final class RangeProduct(start: Long, end: Long) extends RecursiveTask[BigInt] {
    override def compute(): BigInt =
      end - start match {
        case 1 => BigInt(start) * BigInt(end)
        case 2 => BigInt(start) * BigInt(start + 1) * BigInt(end)
        case _ =>
          val mid = (start + end) / 2
          val left = new RangeProduct(start, mid).fork()
          val right = new RangeProduct(mid + 1, end).fork()
          left.join() * right.join()
      }
  }

  /**
    * too slow for huge numbers
    */
  def numOfDigits(n: BigInt): Long = {
    val c = (Math.log(2) / Math.log(10) * n.bitLength + 1).toInt
    val r = BigInt(10).pow(c - 1).compare(n)
    if (r > 0) c - 1 else c
  }

  def numOfFactDigits(n: Long): Long =
    (Math.floor(GammaLn.compute(n + 1) / Math.log(10d)) + 1).longValue

  def stirlingApproximation(n: Int): BigInt = {
    val left = Math.sqrt(2d * Math.PI * n)
    val right = BigDecimal(n).pow(n) * BigDecimal(Math.E).pow(-n)
    val mul = BigDecimal(left) * right
    mul.toBigInt
  }

}

object GammaLn {

  // Lanczos coefficients
  private val lanczos = Array(0.99999999999999709182d,
    57.156235665862923517d, -59.597960355475491248d,
    14.136097974741747174d, -0.49191381609762019978d,
    0.33994649984811888699e-4d, 0.46523628927048575665e-4d,
    -0.98374475304879564677e-4d, 0.15808870322491248884e-3d,
    -0.21026444172410488319e-3d, 0.21743961811521264320e-3d,
    -0.16431810653676389022e-3d, 0.84418223983852743293e-4d,
    -0.26190838401581408670e-4d, 0.36899182659531622704e-5d)

  // value of g constant in the Lanczos approximation
  private val g = 607d / 128d

  def compute(n: Double): Double = {

    if (n.isNaN || n <= 0d)
      return Double.NaN

    val sum = lanczos.indices.map {
      case 0 => lanczos(0)
      case i => lanczos(i) / (n + i)
    }.sum

    val (a, b) = (n + 0.5d, n + g + 0.5d)
    val halfLn2pi = 0.5d * Math.log(2 * Math.PI)

    a * Math.log(b) - b + halfLn2pi + Math.log(sum / n)
  }

}

