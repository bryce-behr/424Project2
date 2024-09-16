import scala.language.postfixOps
import scala.util.Random
import scala.collection.parallel.CollectionConverters.*

// A is a member of the population
class ParGeneticAlgorithm[A](generation: () => A,
                          crossover: (A, A) => A,
                          mutate: (A) => A,
                          score: (A) => Double,
                          population_size: Int,
                          generations: Int,
                          mutation_prob: Double //from 0 to 1.0
                         ) {

  def run(): A = {
    // 1. generate a population using generation in parallel
    var population = (1 to population_size).par.map(_ => generation()).toList

    // percentages of new population
    val TOP_PERFORMERS: Int = 10
    val NEW_RANDOM_MEMBERS: Int = 5
    val CHILDREN: Int = 85

    for (_ <- 0 to generations) {
      // 2. score the population members and choose the best performing
      lazy val scores = population.par.map(A => A -> score(A)).seq.sortBy(-_._2)

      // 3. "breed" the population members using the crossover function to get a new population
      //    - keep top %10
      val topPerformers = scores.take(population_size * TOP_PERFORMERS / 100).toList
      //    - randomize %5
      val newRandomMembers = (0 to (population_size * NEW_RANDOM_MEMBERS / 100)).view.map(_ => generation()).par.map(A => A -> score(A))

      //    - products of crossover function and mutation function that favors high performing members %85
      val children = (0 to (population_size * CHILDREN / 100)).par.map(_ => crossover(selectParent(topPerformers), selectParent(topPerformers)))
      val mutated_children = children.par.map(A => if Random.nextDouble() < mutation_prob then mutate(A) else A)

      //convert to Lists[A] with no score
      // ** for now I'm leaving this in sequential. Parallel might be better idk **
      val topPerformersA = topPerformers.map(item => item._1)
      val newRandomMembersA = newRandomMembers.map(item => item._1)

      //reset population to new performers
      population = topPerformersA ++ newRandomMembersA ++ mutated_children
    }

    //get the final scores and return the best one
    population.par.map(A => (A, score(A))).par.maxBy(_._2)._1
  }

  private def selectParent[A](population: List[(A, Double)]): A = {
    val WEIGHT: Int = 5

    //creates list of random parents and compares them in a tournament style returning the highest
    (0 to WEIGHT).view.map(_ => population(Random.nextInt(population.size))).maxBy(_._2)._1
  }

}
