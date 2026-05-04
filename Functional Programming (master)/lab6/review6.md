% Лабораторная работа № 6а «Введение в объетно-ориентированное
  программирование на языке Scala»
% 20 ноября 2025 г.
% Денис Окутин, ИУ9-11М

# Цель работы
Целью данной работы является изучение базовых объектно-ориентированных 
возможностей языка Scala.

# Индивидуальный вариант
Множество абитуриентов, имеющих оценки по трём ЕГЭ. 
Конструктор множества должен порождать множество, состоящее из одного элемента, 
принимая в качестве параметров имя абитурента, его оценки и факт подачи оригинала
аттестата. Операции: объединение множеств («+»); формирование подмножества абитуриентов,
набравших указанную сумму баллов («/»); формирование подмножества абитуриентов, подавших
оригиналы аттестатов («!»).

# Реализация

```scala
final class ApplicantSet private (private val applicants: Set[ApplicantSet.Applicant]) {
  def +(other: ApplicantSet): ApplicantSet =
    new ApplicantSet(applicants ++ other.applicants)

  def /(totalScore: Int): ApplicantSet =
    new ApplicantSet(applicants.filter(_.total >= totalScore))

  def unary_! : ApplicantSet =
    new ApplicantSet(applicants.filter(_.originalSubmitted))

  override def toString: String =
    applicants.mkString("ApplicantSet(", ", ", ")")
}

object ApplicantSet {
  private case class Applicant(
                                name: String,
                                score1: Int,
                                score2: Int,
                                score3: Int,
                                originalSubmitted: Boolean
                              ) {
    val total: Int = score1 + score2 + score3
    override def toString: String =
      s"$name($score1,$score2,$score3,original=$originalSubmitted)"
  }

  def apply(name: String, score1: Int, score2: Int, score3: Int, originalSubmitted: Boolean): ApplicantSet =
    new ApplicantSet(Set(Applicant(name, score1, score2, score3, originalSubmitted)))
}
```

# Тестирование

```scala
object Main extends App {
  val applicant1 = ApplicantSet("Ivan", 80, 90, 85, originalSubmitted = true) // 255
  val applicant2 = ApplicantSet("Maria", 75, 95, 90, originalSubmitted = false) // 260

  val combined = applicant1 + applicant2
  val highScorers = combined / 256
  val withOriginals = !combined

  println("Объединение: "+combined)
  println("Выше 256 баллов: "+ highScorers)
  println("С оригиналами: "+ withOriginals)
}
```

```
Объединение: ApplicantSet(Ivan(80,90,85,original=true), Maria(75,95,90,original=false))
Выше 256 баллов: ApplicantSet(Maria(75,95,90,original=false))
С оригиналами: ApplicantSet(Ivan(80,90,85,original=true))
```

# Вывод
В ходе выполнения лабораторной работы был разработан класс `ApplicantSet` 
для работы с множествами абитуриентов. Работа позволила освежить навыки 
программирования на Scala,
которые не использовались со времени обучения на третьем курсе.

С технической точки зрения реализация демонстрирует характерные для Scala подходы:
- Применение иммутабельных структур данных соответствует принципам функционального программирования
- Использование методов высшего порядка (`filter`, `++`) вместо рекурсии упрощает код
- Комбинирование ООП и ФП через классы и методы высшего порядка
- Приватный конструктор и объект-компаньон для контроля создания экземпляров

Работа показала, что Scala предоставляет достаточно выразительные средства для
решения задач обработки данных. Операторы `+`, `/` и `!` действительно позволяют 
создавать DSL-подобные конструкции для предметной области.

С практической стороны, реализованный класс решает поставленную задачу - предоставляет 
функциональность для работы с множествами абитуриентов с возможностями фильтрации 
и комбинирования. Код получился компактным и соответствует требованиям задания.

В целом, работа подтвердила, что Scala остается инструментом со своими особенностями,
требующим понимания как объектно-ориентированных, так и функциональных парадигм
программирования.