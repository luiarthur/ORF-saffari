package ORF 

object Template {
  // For regression, keep numClasses = 0
  case class Param(minSamples: Int, minGain: Double, numClasses: Int = 0, numTests: Int = 10, gamma: Double = 0)

  // Sufficient Statistics
  trait SuffStats {
    def n: Int
    def update(y: Double): Unit
  }
  case class ClsSuffStats(counts: Array[Int], private var _n: Int) extends SuffStats {
    def n = _n
    def update(y: Double) = { 
      counts(y.toInt) += 1 
      _n += 1
    }
  }
  case class RegSuffStats(private var _sum: Double, private var _ss: Double, private var _n: Int) extends SuffStats{
    def n = _n
    def update(y: Double) = {
      _sum += y
      _ss += y*y
      _n += 1
    }
  }

  // Decision Tests
  abstract case class Test(dim: Int, loc: Double) {
    val statsL: SuffStats
    val statsR: SuffStats
    def updateL(y: Double): Unit
    def updateR(y: Double): Unit
  }
  class ClsTest(dim: Int, loc: Double, numClasses: Int) extends Test (dim,loc) { // No side effects
    val statsL = ClsSuffStats(Array.fill(numClasses)(0), 0) // fix this
    val statsR = ClsSuffStats(Array.fill(numClasses)(0), 0) // fix this
    def updateL(y: Double) = statsL.update(y)
    def updateR(y: Double) = statsR.update(y)
  }
  class RegTest(dim: Int, loc: Double) extends Test(dim,loc) { // no side effects
    val statsL = RegSuffStats(0,0,0)
    val statsR = RegSuffStats(0,0,0)
    def updateL(y: Double) = statsL.update(y)
    def updateR(y: Double) = statsR.update(y)
  }


  // Elems
  abstract case class Elem(private var _splitDim: Int = -1, private var _splitLoc: Double = 0, numTests: Int = 0) {
    def updateSplit(dim: Int, loc: Double) = { _splitDim=dim; _splitLoc=loc }
    def split = (_splitDim, _splitLoc)
    def reset: Unit
    var stats: SuffStats
    var tests: Vector[Test]
    def update(x: Vector[Double], y: Double): Unit
    def pred: Double
    override def toString = if (_splitDim == -1) pred.toString else "X" + (_splitDim+1) + " < " + (_splitLoc * 100).round / 100.0
  }

  //class ClsElem(override val splitDim: Int, override val splitLoc: Double, override val numSamplesSeen: Int) extends Elem {
  //}

  

  abstract case class ORTree(elem: Elem, param: Param, xrng: Vector[(Double,Double)]) {
    import ORF.Tree

    private var _tree = Tree( elem )
    def tree = _tree

    private var _age = 0
    def age = _age

    private val dimX = xrng.size
    def loss(elem: Elem): Double
    def gains(elem: Elem): Vector[Double]

    val newElem: Elem
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
              j.updateChildren(Tree(newElem), Tree(newElem))
              j.elem.updateSplit(bestTest.dim, bestTest.loc)
              j.left.elem.stats = bestTest.statsL
              j.right.elem.stats = bestTest.statsR
              j.elem.reset
            }
          }
        }
      } else { // k > 0
        // estimate OOBE: Used for Temporal Knowledge Weighting
      }
    }

    private def findLeaf(x: Vector[Double], tree: Tree[Elem]): Tree[Elem] = {
      if (tree.isLeaf) tree else {
        val (dim,loc) = tree.elem.split
        if ( x(dim) > loc ) findLeaf(x, tree.right) else findLeaf(x, tree.left)
      }
    }
    private def poisson(lam: Double) = {
      val Rand = scala.util.Random
      val l = scala.math.exp(-lam)
      def loop(k: Int, p: Double): Int = if (p > l) loop(k+1, p * Rand.nextDouble) else k - 1
      loop(0,1)
    }
  }
}
