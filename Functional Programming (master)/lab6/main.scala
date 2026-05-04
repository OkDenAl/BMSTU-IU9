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