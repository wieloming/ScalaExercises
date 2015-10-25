import scala.annotation.tailrec

case class LearnerParams(target: String, rate: Double, numberOfCandidates: Int)

val chars = ('A' to 'Z') ++ List(' ')
val randomGenerator = new scala.util.Random

def randomChar = chars(randomGenerator.nextInt(chars.size))

implicit class PopulationList[T](t: List[String]) {
  def fitest = t.sortBy(fitness)(Ordering[Int].reverse).head
  private def fitness(candidate: String)(implicit params: LearnerParams): Int =
    (candidate zip params.target)
      .map { case (candidateChar, targetChat) => if (candidateChar == targetChat) 1 else 0 }
      .sum
}

def mutate(initial: String)(implicit params: LearnerParams) =
  initial
    .map(if (randomGenerator.nextDouble < params.rate) randomChar else _)

@tailrec
def evolve(generation: Int, actualForm: String)(implicit params: LearnerParams) {
  import params._
  printf("Generation: %3d  %s\n", generation, actualForm)
  if (actualForm == target) return
  // elitism = at least one best solution must be copied to new population
  val candidates = actualForm :: (1 to numberOfCandidates).map(_ => mutate(actualForm)).toList
  val next = candidates.fitest
  evolve(generation + 1, next)
}

implicit val params = LearnerParams("METHINKS IT IS LIKE A WEASEL", 0.01, 100)
val initial = (1 to params.target.length).map(x => randomChar).mkString
evolve(0, initial)