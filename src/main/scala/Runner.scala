import java.io.FileWriter
import com.opencsv.CSVWriter

@main def main(): Unit = {
/*  //Define constants
  val numTests = 10
  val popSize = 10
  val numGenerations = 10
  val mutationProb = 1*/

  getTestData()

/*  //initialize the sequential algo and get results of timeIt
  val seqSalesman = GeneticAlgorithm(WanderingSalesman.generation, WanderingSalesman.crossover, WanderingSalesman.mutate, WanderingSalesman.score, popSize, numGenerations, mutationProb)
  val seqTuple = for(i <- 0 until numTests) yield timeIt(seqSalesman.run())

  //initialize the parallel algo and get results of timeIt
  val parSalesman = ParGeneticAlgorithm(WanderingSalesman.generation, WanderingSalesman.crossover, WanderingSalesman.mutate, WanderingSalesman.score, popSize, numGenerations, mutationProb)
  val parTuple = for (i <- 0 until numTests) yield timeIt(parSalesman.run())

  //put all results in one list
  val seqResults = seqTuple.map(i => (i._1, i._2, WanderingSalesman.score(i._2)))
  val parResults = parTuple.map(i => (i._1, i._2, WanderingSalesman.score(i._2)))

  //get average times
  val avgSeqTime = seqResults.foldLeft(0.0)(_+_._1) / seqResults.length
  val avgParTime = parResults.foldLeft(0.0)(_ + _._1) / parResults.length

  //get average scores
  val avgSeqScore = seqResults.foldLeft(0.0)(_+_._3) / seqResults.length
  val avgParScore = parResults.foldLeft(0.0)(_ + _._3) / parResults.length

  //print averages
  println(s"Sequential Averages:\nTime: $avgSeqTime\tScore: $avgSeqScore\n")
  println(s"Parellel Averages:\nTime: $avgParTime\tScore: $avgParScore")

  //write to a csv
  val csvFile = "data.csv"
  val writer = new CSVWriter(new FileWriter(csvFile))

  writer.writeNext(Array("Sequential"))
  writer.writeNext(seqResults.map(_._1.toString).toArray)
  writer.writeNext(seqResults.map(_._3.toString).toArray)

  writer.writeNext(Array())
  writer.writeNext(Array("Parallel"))

  writer.writeNext(parResults.map(_._1.toString).toArray)
  writer.writeNext(parResults.map(_._3.toString).toArray)

  writer.close()

  println("\nData written to CSV successfully.")*/
}

def getTestData(): Unit = {
  //Define constants
  val numTests = 10
//  val popSize = 10
  val numGenerations = 10
  val mutationProb = 1

  //write to a csv
  val csvFile = "data.csv"
  val writer = new CSVWriter(new FileWriter(csvFile))

  for(i <- Range.Inclusive(1000, 10000, 500)) do {
    //initialize the sequential algo and get results of timeIt
    val seqSalesman = GeneticAlgorithm(WanderingSalesman.generation, WanderingSalesman.crossover, WanderingSalesman.mutate, WanderingSalesman.score, i, numGenerations, mutationProb)
    val seqTuple = for(i <- 0 until numTests) yield timeIt(seqSalesman.run())

    //initialize the parallel algo and get results of timeIt
    val parSalesman = ParGeneticAlgorithm(WanderingSalesman.generation, WanderingSalesman.crossover, WanderingSalesman.mutate, WanderingSalesman.score, i, numGenerations, mutationProb)
    val parTuple = for (i <- 0 until numTests) yield timeIt(parSalesman.run())

    //get average times
    val avgSeqTime = seqTuple.foldLeft(0.0)(_+_._1) / seqTuple.length
    val avgParTime = parTuple.foldLeft(0.0)(_ + _._1) / parTuple.length


    val percentChange = (avgParTime-avgSeqTime) / avgSeqTime

    writer.writeNext(Array(i.toString, percentChange.toString))
  }
  writer.close()

  println("\nData written to CSV successfully.")
}