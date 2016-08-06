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
  case class Param(minSamples: Int, minGain: Double, xrng: Vector[(Double,Double)], numClasses: Int, numTests: Int = 10, gamma: Double = 0)

  // Sufficient Statistics
  trait SuffStats {
    def n: Int
    def update(y: Double): Unit
    def reset: Unit
    def pred: Double
  }
  case class ClsSuffStats(private var _counts: Array[Int], private var _n: Int) extends SuffStats {
    def n = _n
    def counts = _counts
    def update(y: Double) = { 
      _counts(y.toInt) += 1 
      _n += 1
    }
    def reset = { _counts=Array(); _n=0 }
    def pred = _counts.zipWithIndex.maxBy(_._1)._2
    override def toString = counts.mkString(",") + " | " + n
  }
  case class RegSuffStats(private var _sum: Double, private var _ss: Double, private var _n: Int) extends SuffStats {
    def n = _n
    def sum = _sum
    def ss  = _ss
    def update(y: Double) = {
      _sum += y
      _ss += y*y
      _n += 1
    }
    def reset = { _sum=0; _ss=0; _n=0 }
    def pred = 1 // FIXME
  }

  // Decision Tests
  abstract case class Test(dim: Int, loc: Double) {
    val statsL: SuffStats
    val statsR: SuffStats
    def update(x: Vector[Double], y: Double) = if (x(dim) < loc) statsL.update(y) else statsR.update(y)
  }
  class ClsTest(dim: Int, loc: Double, numClasses: Int) extends Test (dim,loc) { // No side effects
    val statsL = ClsSuffStats(Array.fill(numClasses)(1), 0)
    val statsR = ClsSuffStats(Array.fill(numClasses)(1), 0)
  }
  class RegTest(dim: Int, loc: Double) extends Test(dim,loc) { // no side effects
    val statsL = RegSuffStats(0,0,0)
    val statsR = RegSuffStats(0,0,0)
  }


  // Elems
  abstract case class Elem(private var _splitDim: Int = -1, private var _splitLoc: Double = 0, private var _numSamplesSeen: Int = 0) {
    def updateSplit(dim: Int, loc: Double) = { _splitDim=dim; _splitLoc=loc }
    def split = (_splitDim, _splitLoc)
    def reset: Unit 

    def stats: SuffStats
    def stats_=(newStats: Any): Unit
    //var stats: SuffStats

    def tests: Vector[Test]
    def update(x: Vector[Double], y: Double) = { 
      stats.update(y)
      tests foreach { _.update(x,y) }
    }
    def pred = stats.pred
    //def reset: Unit
    override def toString = if (_splitDim == -1) pred.toString else "X" + (_splitDim+1) + " < " + (_splitLoc * 100).round / 100.0
  }

  class ClsElem(var _splitDim: Int, var _splitLoc: Double, var _numSamplesSeen: Int, val param: Param) extends Elem {
    //def reset = stats.
    val dimX = param.xrng.size
    private var _tests = Vector.range(0,param.numTests) map {t => generateTest}
    def tests = _tests

    private var _stats = ClsSuffStats(Array.fill(param.numClasses)(1),0)
    def stats = _stats
    def stats_=(newStats: Any) = newStats match {
      case ns: ClsSuffStats => _stats = ns
      case _ => println("Something is wrong")
    }

    def generateTest = {
      val dim = Rand.nextInt(dimX)
      val loc = runif(param.xrng(dim))
      new ClsTest(dim, loc, param.numClasses)
    }

    def reset = {
      _stats.reset
      _tests = Vector()
    }
    //def dens = c.map { cc => cc / (numSamples.toDouble + numClasses) }
  }

  

  abstract case class ORTree(elem: Elem, param: Param) {
    import ORF.Tree

    def loss(suff: Any): Double
    def updateOOBE(x: Vector[Double], y: Double): Unit
    def newElem: Elem
    //def reset: Unit

    private def gains[E <: Elem](elem: E): Vector[Double] = {
      val tests = elem.tests
      val s = elem.stats
      tests map { test =>
        val sL = test.statsL
        val nL = sL.n + param.numClasses
        val sR = test.statsR
        val nR = sR.n + param.numClasses
        val n = (nL + nR).toDouble
        val g = loss(s) - (nL/n) * loss(sL) - (nR/n) * loss(sR)
        if (g < 0) 0 else g
      }
    }

    private var _tree = Tree( elem )
    def tree = _tree

    private var _age = 0
    def age = _age

    private val dimX = param.xrng.size

    def update(x: Vector[Double], y: Double) {
      val k = poisson(1)
      if (k > 0) {
        for (u <- 1 to k) {
          _age += 1
          val j = findLeaf(x,_tree)
          j.elem.update(x,y)
          if (j.elem.stats.n > param.minSamples) {
            val g = gains(j.elem)
            if ( g.exists(_ > param.minGain) ) {
              val bestTest = g.zip(j.elem.tests).maxBy(_._1)._2
              // create Left, Right children
              j.elem.updateSplit(bestTest.dim, bestTest.loc)
              j.updateChildren(Tree(newElem), Tree(newElem))
              j.left.elem.stats  = bestTest.statsL
              j.right.elem.stats = bestTest.statsR
              //j.elem.reset // FIXME
            }
          }
        }
      } else { // k > 0
        // estimate OOBE: Used for Temporal Knowledge Weighting
        updateOOBE(x,y)
      }
    }

    def findLeaf(x: Vector[Double], tree: Tree[Elem]): Tree[Elem] = {
      if (tree.isLeaf) tree else {
        val (dim,loc) = tree.elem.split
        if ( x(dim) > loc ) findLeaf(x, tree.right) else findLeaf(x, tree.left)
      }
    }
    def predict(x: Vector[Double]) = findLeaf(x,tree).elem.pred
    private def poisson(lam: Double) = {
      val l = scala.math.exp(-lam)
      def loop(k: Int, p: Double): Int = if (p > l) loop(k+1, p * Rand.nextDouble) else k - 1
      loop(0,1)
    }
  }
}
