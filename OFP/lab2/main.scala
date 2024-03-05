class InequalitySystem (coefficients: List[List[Double]], constant: List[Double]) {
  var coef: List[List[Double]] = coefficients
  val const = constant

  def this(coefficients: List[Double], constant: Double) = this(List(coefficients), List(constant))


  def +(other: InequalitySystem): InequalitySystem = {
    new InequalitySystem(this.coef:::other.coef, this.const:::other.const)
  }

  def /(i: Int): Unit = {
    coef = coef.map(l => l.updated(i, 0.0))
  }

  def check(values: List[Double]): Boolean = {
    var i = -1
    coef.forall(l => {
      i += 1
      l.zip(values).map { case (a, x) => a * x }.sum <= const(i)
    })
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val l = new InequalitySystem(List(List(1.0, 2.0, 3.0),List(1.0, 2.0, 15.0)), List(10.0,9.0))
    l/(1)
    println(l.coef)
    val p =  l.check(List(1,1,1))
    println(p)
    val r = new InequalitySystem(List(List(3.0, 4.0, 3.0),List(9.0, 2.0, 15.0)), List(110.0,90.0))
    val e = l+r
    println(e.coef)
  }
}

