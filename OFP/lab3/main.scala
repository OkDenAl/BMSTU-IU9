trait Addable[T] {
  def add(x: T, y: T): T
}

object Addable {
  implicit object IntAddable extends Addable[Int] {
    def add(x: Int, y: Int): Int = x + y
  }

  implicit object StringAddable extends Addable[String] {
    def add(x: String, y: String): String = x + y
  }
}

class Fib[T](val current: T, val nextVal: T)(implicit ev: Addable[T]) {
  def next(): Fib[T] = new Fib[T](nextVal, ev.add(current, nextVal))
}


object Main {
  private def printNFib[T](start1: T, start2: T, n: Int)(implicit ev: Addable[T]): Unit = {
    var fib = new Fib[T](start1, start2)
    for (i <- 1 to n) {
      printf("\t%d-й член Фибоначчи: %s\n",i,fib.current)
      fib = fib.next()
    }
  }

  def main(args: Array[String]): Unit = {
    println("Последовательность Фибоначчи целых чисел:")
    printNFib[Int](0, 1, 10)

    println("\nПоследовательность Фибоначчи строк:")
    printNFib[String]("A", "B", 5)
  }
}