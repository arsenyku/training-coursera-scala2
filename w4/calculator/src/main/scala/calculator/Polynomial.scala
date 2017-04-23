package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
  {
    Signal( (b()*b()) - (4*a()*c()) )
    // (b*b) + (4*a*c)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    // (-b +- sqrt(delta))/2a
    Signal(computeRoots(a(),b(),c(),delta()))
  }

  def computeRoots(a: Double, b: Double, c:Double, delta: Double): Set[Double] =
  {
    if ( delta < 0 ) Set()
    else
    {
      val root1 = (-b + Math.sqrt(delta)) / (2 * a)
      val root2 = (-b - Math.sqrt(delta)) / (2 * a)
      Set(root1, root2)
    }
  }
}
