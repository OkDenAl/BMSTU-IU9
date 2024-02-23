val mul: (List[Int], List[Int]) => List[Int] = {
  case (Nil,Nil) => Nil
  case (Nil, list2) => list2
  case (list1, Nil) => list1
  case (x :: xs, y ::ys) => (x + y)::mul(xs,ys)
}

object Main extends App {
  val num1 = List() // Число 48 = 2^2 * 3^1*4^1
  val num2 = List(1, 2) // Число 18 = 2^1 * 3^2

  val result = mul(num1, num2)
  println(result)
}
