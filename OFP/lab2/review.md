% Лабораторная работа № 2 «Введение в объектно-ориентированное программирование
  на языке Scala»
% 6 марта 2024 г.
% Денис Окутин, ИУ9-61Б

# Цель работы
Целью данной работы является изучение базовых объектно-ориентированных 
возможностей языка Scala.

# Индивидуальный вариант
Система неравенств вида a1x1+a2x2+…+aNxN≤b. Конструктор класса должен принимать список 
коэффициентов ai и свободный член b и порождать систему, состоящую из одного неравенства.
Операции: «+» — объединение двух систем в одну; «/» — принимает число i и возвращает систему,
полученную из данной системы путём присвоения нулевого значения i-й переменной; 
«check» — проверка, удовлетворяет ли список значений переменных системе неравенств.

# Реализация и тестирование

```scala
class InequalitySystem (coefficients: List[List[Double]], constant: List[Double]) {
  var coef: List[List[Double]] = coefficients
  val const = constant

  def this(coefficients: List[Double], constant: Double) = this(List(coefficients), List(constant))


  def +(other: InequalitySystem): InequalitySystem = {
    new InequalitySystem(this.coef:::other.coef, this.const:::other.const)
  }

  def /(i: Int): InequalitySystem = {
    new InequalitySystem(coef.map(l => l.patch(i, Nil, 1)), const)
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
    val a=l/(5)
    println(a.coef)
    val p =  a.check(List(10))
    println(p)
    val r = new InequalitySystem(List(List(3.0, 4.0, 3.0),List(9.0, 2.0, 15.0)), List(110.0,90.0))
    val e = l+r
    println(e.coef)
  }
}
```

Вывод программы:

```scala
List(List(1.0, 0.0, 3.0), List(1.0, 0.0, 15.0))
false
List(List(1.0, 0.0, 3.0), List(1.0, 0.0, 15.0), List(3.0, 4.0, 3.0), List(9.0, 2.0, 15.0))
```

# Вывод
В ходе данной лабораторной работы было проведено ознакомление с базовыми
объектно-ориентированными возможностями языка программирования Scala,
получен опыт разработки классов.
Также были изучены возможности контейнерных классов, доступных в стандартной библиотеке
языка Scala.