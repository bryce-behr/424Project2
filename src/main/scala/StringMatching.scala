import scala.util.Random

object StringMatching {
  def generation(): String = {
    val alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    def randStr(n: Int) = (1 to n).map(_ => alpha(Random.nextInt(alpha.length))).mkString
    randStr(10)
  }
  def crossover(parent1: String, parent2: String): String = {
    parent1.substring(0, 5) + parent2.substring(5, 10)
  }
  def mutate(s: String): String = {
    val alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    s.map(c => if (Random.nextInt(100) < 5) then alpha(Random.nextInt(alpha.length)) else c)
  }
  def score(s: String): Double = {
    val target = "iHateGenes"
    var score: Int = 0

    for (i <- 0 to s.length - 1) {
      if target(i) == s(i) then score += 1
    }

    score.toDouble
  }
}