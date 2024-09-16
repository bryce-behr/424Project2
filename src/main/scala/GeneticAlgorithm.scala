import scala.language.postfixOps
import scala.util.Random

// A is a member of the population
class GeneticAlgorithm[A](generation: () => A,
                          crossover: (A, A) => A,
                          mutate: (A) => A,
                          score: (A) => Double,
                          population_size: Int,
                          generations: Int,
                          mutation_prob: Double //from 0 to 1.0
  ) {

  def run(): A = {
    // 1. generate a population using generation
    var population = List.fill(population_size)(generation())

    // percentages of new population
    val TOP_PERFORMERS: Int = 10
    val NEW_RANDOM_MEMBERS: Int = 5
    val CHILDREN: Int = 85

    for (_ <- 0 to generations) {
      // 2. score the population members and choose the best performing
      val scores = population.map(A => A -> score(A)).seq.sortBy(-_._2)

      // 3. "breed" the population members using the crossover function to get a new population
      //    - keep top %10
      val topPerformers = scores.take(population_size * TOP_PERFORMERS / 100)
      //    - randomize %5
      val newRandomMembers = (0 to (population_size * NEW_RANDOM_MEMBERS / 100)).map(_ => generation()).map(A => A -> score(A))

      //    - products of crossover function and mutation function that favors high performing members %85
      val children = (0 to (population_size * CHILDREN / 100)).map(_ => crossover(selectParent(topPerformers), selectParent(topPerformers)))
      val mutated_children = children.map(A => if (Random.nextDouble() < mutation_prob) then mutate(A) else A)

      //convert to Lists[A] with no score
      val topPerformersA = topPerformers.map(item => item._1)
      val newRandomMembersA = newRandomMembers.map(item => item._1)

      //reset population to new performers
      population = topPerformersA ++ newRandomMembersA ++ mutated_children
    }

    //get the final scores and return the best one
    val final_pop_scores = population.map(A => (A, score(A)))
    final_pop_scores.maxBy(_._2)._1

  }

  private def selectParent[A](population: List[(A, Double)]): A = {
    val WEIGHT: Int = 5

    //creates list of random parents and compares them in a tournament style returning the highest
    (0 to WEIGHT).map(_ => population(Random.nextInt(population.size))).maxBy(_._2)._1
  }

}
def timeIt[A](f: => A): (Double, A) = {
  val startTime = System.currentTimeMillis()
  val result = f
  val endTime = System.currentTimeMillis()
  (endTime-startTime, result)
}
/*@main def main(): Unit = {
  //val stringMatching = GeneticAlgorithm(StringMatching.generation, StringMatching.crossover, StringMatching.mutate, StringMatching.score, 1000, 100, 100)
  //print(stringMatching.run())

  val wanderingSalesman = GeneticAlgorithm(WanderingSalesman.generation, WanderingSalesman.crossover, WanderingSalesman.mutate, WanderingSalesman.score, 10000, 10, 100)
  val parWanderingSalesman = ParGeneticAlgorithm(WanderingSalesman.generation, WanderingSalesman.crossover, WanderingSalesman.mutate, WanderingSalesman.score, 10000, 10, 100)
  //print(WanderingSalesman.score(wanderingSalesman.run())) // closer to 0 the better

  var sAvg: Double = 0
  for (_ <- 0 to 5) {
    val (time1, cords1) = timeIt(wanderingSalesman.run())
    sAvg = sAvg + time1
//    print(s"[${time1} ms] ")
//    println(s"Sequential salesman score: ${WanderingSalesman.score(cords1)}")
  }
  println(s"Average sequential time: [${sAvg / 5} ms] ")

  var pAvg: Double = 0
  for (_ <- 0 to 5) {
    val (time2, cords2) = timeIt(parWanderingSalesman.run())
    pAvg = pAvg + time2
//    print(s"[${time2} ms] ")
//    println(s"Parallel salesman score: ${WanderingSalesman.score(cords2)}")
  }
  println(s"Average parallel time: [${pAvg / 5} ms] ")


}*/
