package ORF

object NodeElem {

  /** holds info of nodes of online random tree */
  case class Elem(splitDim: Int= -1, splitLoc: Double=0, numSamplesSeen: Int=0, counts: Vector[Int]=Vector(), tests: Vector[Test]=Vector()) {
    def total = counts.sum.toDouble
    def numClasses = counts.size
    def pred = counts.zipWithIndex.maxBy(_._1)._2 // should use dens
    def dens = counts.map { cc => cc / (total + numClasses) }
    def update(x: Vector[Double], y: Int) = {
      val newCounts = counts.updated(y, counts(y)+1)
      val newNumSamplesSeen = numSamplesSeen + 1
      val newTests = tests map { test =>
        val dim = test.dim
        val loc = test.loc
        if (x(dim) < loc) {
          val newCountsL = test.countsLeft.updated(y, test.countsLeft(y)+1) 
          test.copy(countsLeft = newCountsL)
        } else {
          val newCountsR = test.countsRight.updated(y, test.countsRight(y)+1) 
          test.copy(countsRight = newCountsR)
        }
      }
      val newElem = this.copy(tests = newTests, counts = newCounts, numSamplesSeen = newNumSamplesSeen)
      newElem
    }
    override def toString = if (splitDim == -1) pred.toString else "X" + (splitDim+1) + " < " + (splitLoc * 100).round / 100.0
  }

  case class Test(dim: Int, loc: Double, countsLeft: Vector[Int], countsRight: Vector[Int])
  def generateTests(numTests: Int, numClasses: Int, xrng: Vector[(Double,Double)]) = {
    val Rand = scala.util.Random
    val dimX = xrng.size
    def runif(rng: (Double,Double)) = Rand.nextDouble * (rng._2-rng._1) + rng._1
    def gentest = {
      val dim = Rand.nextInt(dimX)
      val loc = runif(xrng(dim))
      val cL  = Vector.fill(numClasses)(1)
      val cR  = Vector.fill(numClasses)(1)
      Test(dim,loc,cL,cR)
    }
    Vector.range(0, numTests) map {s => gentest}
  }
}
