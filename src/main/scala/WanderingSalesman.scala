import scala.math._
import scala.util.Random

object WanderingSalesman {
  final val CORD_SIZE: Int = 100 // the x and y bound of where a destination can generate
  private final val NUM_DESTINATIONS: Int = 100 // how many points the salesman will need to go to
  private final val MUTATION_PROB: Double = .05 // how often mutations happen (MUTATION_PROB * 100)%
  private final val SEED_ORDERING: List[(Double, Double)] = (0 to NUM_DESTINATIONS).map(_ => Random.nextDouble() * CORD_SIZE -> Random.nextDouble() * CORD_SIZE).toList
  def generation(): List[(Double, Double)] = {
    //generates and ordering of coordinates based on final variable
    Random.shuffle(SEED_ORDERING)
  }

  def crossover(coordinates1: List[(Double, Double)], coordinates2: List[(Double, Double)]): List[(Double, Double)] =  {
    // chose a random sequence of 1 to insert in 2
    val i1, i2 = Random.nextInt(coordinates1.size)
    val start = min(i1, i2)
    val end = max(i1, i2)

    val list1 = coordinates2.slice(0, start)
    val list2 = coordinates1.slice(start, end)
    val list3 = coordinates2.slice(end, coordinates2.size)

    list1 ++ list2 ++ list3
  }
  def mutate(coordinates: List[(Double, Double)]): List[(Double, Double)] = {
    var cords = coordinates

    for (i <- 0 to coordinates.size - 1) yield if Random.nextDouble() <= 0.05 then cords = swap(cords, i)

    def swap(coordinates: List[(Double, Double)], i1: Int): List[(Double, Double)] = {
      val randomIndex = Random.nextInt(coordinates.length)
      val temp = coordinates(i1)
      coordinates.updated(i1, coordinates(randomIndex)).updated(randomIndex, temp)
    }

    cords
  }

  def score(coordinates: List[(Double, Double)]): Double = {
    //gets distance between two points
    def distance(coord1: (Double, Double), coord2: (Double, Double)): Double = sqrt(pow(coord2._1 - coord1._1, 2) + pow(coord2._2 - coord2._2, 2))

    //calculates total distance between points in the sequence
    // negative because GeneticAlgorithm ranks higher scores as better
    coordinates
      .sliding(2)
      .map(item => distance(item(0), item(1)))
      .sum * -1
  }

}
