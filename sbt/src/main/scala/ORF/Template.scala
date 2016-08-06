package ORF 

object Template {
   val Rand = scala.util.Random
   private def runif(rng: (Double,Double)) = Rand.nextDouble * (rng._2-rng._1) + rng._1

  /** Parameters for the OR-Tree, OR-Foest
   *  @constructor create a set of parameters for ORT / ORF
   *  @param numClasses  number of classes in response. e.g. if numClasses == 3, then the responses should be one of {0,1,2}. For regression, set to 0.
   *  @param minSamples  the minimum number of samples a node needs to see before it is permitted to split
   *  @param minGain  the minimum gain (based on metric) that a split needs to achieve before split can occur
   *  @param gamma  the temporal weighting learning rate (>0). if age of tree > 1/gamma, the tree is thrown away. (default=0)
   *  @param numTests  the number of tests (split dimension, location) each node does. (default=10)
   *  @param xrng
   */
  case class Param(minSamples: Int, minGain: Double, xrng: Vector[(Double,Double)], numClasses: Int = 0, numTests: Int = 10, gamma: Double = 0)

  // Sufficient Statistics
  trait SuffStats {
    def n: Int
    def update(x: Vector[Double], y: Double): Unit
  }
  case class ClsSuffStats(counts: Array[Int], private var _n: Int) extends SuffStats {
    def n = _n
    def update(x: Vector[Double], y: Double) = { 
      counts(y.toInt) += 1 
      _n += 1
    }
  }
  case class RegSuffStats(private var _sum: Double, private var _ss: Double, private var _n: Int) extends SuffStats {
    def n = _n
    def update(x: Vector[Double], y: Double) = {
      _sum += y
      _ss += y*y
      _n += 1
    }
  }

  // Decision Tests
  abstract case class Test(dim: Int, loc: Double) {
    val statsL: SuffStats
    val statsR: SuffStats
    def updateL(x: Vector[Double], y: Double) = statsL.update(x,y)
    def updateR(x: Vector[Double], y: Double) = statsR.update(x,y)

  }
  class ClsTest(dim: Int, loc: Double, numClasses: Int) extends Test (dim,loc) { // No side effects
    val statsL = ClsSuffStats(Array.fill(numClasses)(0), 0) // fix this
    val statsR = ClsSuffStats(Array.fill(numClasses)(0), 0) // fix this
  }
  class RegTest(dim: Int, loc: Double) extends Test(dim,loc) { // no side effects
    val statsL = RegSuffStats(0,0,0)
    val statsR = RegSuffStats(0,0,0)
  }


  // Elems
  abstract case class Elem(private var _splitDim: Int = -1, private var _splitLoc: Double = 0, private var _numSamplesSeen: Int = 0) {
    def updateSplit(dim: Int, loc: Double) = { _splitDim=dim; _splitLoc=loc }
    def split = (_splitDim, _splitLoc)
    //def reset: Unit
    def stats: SuffStats
    def tests: Vector[Test]
    def update(x: Vector[Double], y: Double): Unit
    def pred: Double
    //def reset: Unit
    override def toString = if (_splitDim == -1) pred.toString else "X" + (_splitDim+1) + " < " + (_splitLoc * 100).round / 100.0
  }

  class ClsElem(var _splitDim: Int, var _splitLoc: Double, var _numSamplesSeen: Int, val param: Param) extends Elem {
    //def reset = stats.
    val dimX = param.xrng.size
    private var _stats = ClsSuffStats(Array.fill(param.numClasses)(0),0)
    private var _tests = Vector.range(0,param.numTests) map {t => generateTest}
    def stats = _stats
    def tests = _tests
    def pred = stats.counts.zipWithIndex.maxBy(_._1)._2
    def reset = { 
      _stats = ClsSuffStats(Array(),0) 
      _tests = Vector()
    }
    def update(x: Vector[Double], y: Double) = {
      
    }

    def generateTest = {
      val dim = Rand.nextInt(dimX)
      val loc = runif(param.xrng(dim))
      new ClsTest(dim, loc, param.numClasses)
    }
  }

  

  abstract case class ORTree(elem: Elem, param: Param, xrng: Vector[(Double,Double)]) {
    import ORF.Tree

    private var _tree = Tree( elem )
    def tree = _tree
    def reset: Unit

    private var _age = 0
    def age = _age

    private val dimX = xrng.size
    def loss(elem: Elem): Double
    def gains(elem: Elem): Vector[Double]
    def updateOOBE(x: Vector[Double], y: Double): Unit

    val newElem: Elem
    def update(x: Vector[Double], y: Double) {
      val k = poisson(1)
      if (k > 0) {
        for (u <- 1 to k) {
          _age += 1
          val j = findLeaf(x,_tree)
          j.elem.update(y)
          if (j.elem.stats.n > param.minSamples) {
            val g = gains(j.elem)
            if ( g.exists(_ > param.minGain) ) {
              val bestTest = g.zip(j.elem.tests).maxBy(_._1)._2
              // create Left, Right children
              j.updateChildren(Tree(newElem), Tree(newElem))
              j.elem.updateSplit(bestTest.dim, bestTest.loc)
              //j.left.elem.stats = bestTest.statsL
              //j.right.elem.stats = bestTest.statsR
              //j.elem.reset
            }
          }
        }
      } else { // k > 0
        // estimate OOBE: Used for Temporal Knowledge Weighting
        updateOOBE(x,y)
      }
    }

    private def findLeaf(x: Vector[Double], tree: Tree[Elem]): Tree[Elem] = {
      if (tree.isLeaf) tree else {
        val (dim,loc) = tree.elem.split
        if ( x(dim) > loc ) findLeaf(x, tree.right) else findLeaf(x, tree.left)
      }
    }
    private def poisson(lam: Double) = {
      val l = scala.math.exp(-lam)
      def loop(k: Int, p: Double): Int = if (p > l) loop(k+1, p * Rand.nextDouble) else k - 1
      loop(0,1)
    }
  }
}
