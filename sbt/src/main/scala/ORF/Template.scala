package ORF 

object Template {
  private val Rand = scala.util.Random
  private def runif(rng: (Double,Double)) = Rand.nextDouble * (rng._2-rng._1) + rng._1
  import ORF.models._

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
    private val eps = 1E-10
    def reset = { _sum=0; _ss=0; _n=0 }
    def pred = sum / (n+eps)
    def sd = scala.math.sqrt( (ss/(n+eps) - pred*pred) )
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
  abstract case class Elem(private var _splitDim: Int = -1, private var _splitLoc: Double = 0, private var _numSamplesSeen: Int = 0, param: Param) {
    protected val dimX = param.xrng.size
    def updateSplit(dim: Int, loc: Double) = { _splitDim=dim; _splitLoc=loc }
    def split = (_splitDim, _splitLoc)
    def reset: Unit 
    def stats: SuffStats
    def stats_=(newStats: Any): Unit
    def numSamplesSeen = _numSamplesSeen

    def tests = _tests
    def update(x: Vector[Double], y: Double) = { 
      stats.update(y)
      tests foreach { _.update(x,y) }
      _numSamplesSeen += 1
    }
    def pred = stats.pred
    override def toString = if (_splitDim == -1) pred.toString else "X" + (_splitDim+1) + " < " + (_splitLoc * 100).round / 100.0

    protected def generateTest: Test
    protected var _tests = Vector.range(0,param.numTests) map {t => generateTest}
  }

  class ClsElem(var _splitDim: Int, var _splitLoc: Double, var _numSamplesSeen: Int, override val param: Param) extends Elem(param=param) {
    def stats = _stats
    def stats_=(newStats: Any) = newStats match { case ns: ClsSuffStats => _stats = ns }
    def reset = {
      _stats.reset
      _tests = Vector()
    }
    def dens = stats.counts.map { c => c / (stats.n.toDouble + param.numClasses) }

    private var _stats = ClsSuffStats(Array.fill(param.numClasses)(1),0)
    protected def generateTest = {
      val dim = Rand.nextInt(dimX)
      val loc = runif(param.xrng(dim))
      new ClsTest(dim, loc, param.numClasses)
    }
  }

  class RegElem(var _splitDim: Int, var _splitLoc: Double, var _numSamplesSeen: Int, override val param: Param) extends Elem(param=param) {
    def stats = _stats
    def stats_=(newStats: Any) = newStats match { case ns: RegSuffStats => _stats = ns }
    def reset = {
      _stats.reset
      _tests = Vector()
    }

    private var _stats = RegSuffStats(0,0,1)
    protected def generateTest = {
      val dim = Rand.nextInt(dimX)
      val loc = runif(param.xrng(dim))
      new RegTest(dim, loc)
    }
  }

  // ORTree
  abstract case class ORTree(elem: Elem, param: Param) {
    import ORF.Tree

    // Protected to implement in subclass of ORTree. Protected methods are seen only by subclasses.
    protected def updateOOBE(x: Vector[Double], y: Double): Unit
    protected def newElem: Elem
    def reset: Unit
    def oobe: Double

    // Public methods
    def age = _age
    def tree = _tree
    def predict(x: Vector[Double]) = findLeaf(x,tree).elem.pred
    def update(x: Vector[Double], y: Double) {
      val k = poisson(1)
      if (k > 0) {
        for (u <- 1 to k) {
          _age += 1
          val j = findLeaf(x,_tree)
          j.elem.update(x,y)
          if (j.elem.numSamplesSeen > param.minSamples) { //FIXME: which one is the correct approach?
          //if (j.elem.stats.n > param.minSamples) {
            val g = gains(j.elem)
            if ( g.exists(_ > param.minGain) ) {
              val bestTest = g.zip(j.elem.tests).maxBy(_._1)._2
              // create Left, Right children
              j.elem.updateSplit(bestTest.dim, bestTest.loc)
              j.updateChildren(Tree(newElem), Tree(newElem))
              j.left.elem.stats  = bestTest.statsL
              j.right.elem.stats = bestTest.statsR
              j.elem.reset
            }
          }
        }
      } else { // k > 0
        // estimate OOBE: Used for Temporal Knowledge Weighting when param.gamma > 0
        updateOOBE(x,y)
      }
    }

    // Private fields
    protected var _age = 0
    protected var _tree = Tree( elem )
    private val dimX = param.xrng.size

    // Private methods
    private def loss[S <: SuffStats](suff: S): Double = suff match {
      case s: ClsSuffStats => {
        def log2(x: Double) = scala.math.log(x) / scala.math.log(2)
        val n = s.counts.sum.toDouble// + param.numClasses
        (s.counts map { x => val p = x / n; -p * log2(p) }).sum
      }
      case r: RegSuffStats => r.sd
      case _ => 0.0
    }
    private def gains[E <: Elem](elem: E): Vector[Double] = {
      val tests = elem.tests
      val s = elem.stats
      tests map { test =>
        val sL = test.statsL
        val nL = sL.n //+ param.numClasses
        val sR = test.statsR
        val nR = sR.n //+ param.numClasses
        val n = (nL + nR + 1E-10).toDouble
        assert(n > 0)
        val g = loss(s) - (nL/n) * loss(sL) - (nR/n) * loss(sR)
        if (g < 0) 0 else g
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

  /** ORForest: Online Random Forest for Classification.
   *  @constructor creates online random forest object
   *  @param param parameter settings for random forest
   *  @param xrng range for each column of X matrix. ( you can use Tools.dataRange(X) to get xrng )
   *  @param numTrees number of trees in forest
   *  @param par if true, trees in forest are updated in parallel. Otherwise, trees in forest are updated sequentially.
   */
  abstract case class ORForest(param: Param, numTrees: Int = 100, par: Boolean = false) {
    private val Rand = scala.util.Random

    // Methods to define
    def newORTree: ORTree

    /** predict / classify based on input x */
    def predict(x: Vector[Double]): Double

    // Other methods & fields
    def predicts(xs: Vector[Vector[Double]]) = xs map predict 
    private val xrng = param.xrng
    protected val _forest = {
      val f = Vector.range(1,numTrees) map { i => 
        val tree = newORTree
        tree
      }
      if (par) f.par else f
    }
    def forest = _forest

    /** update the random forest based on new observations x, y */
    def update(x: Vector[Double], y: Double): Unit = {
      _forest.foreach( _.update(x,y) )
      if (param.gamma > 0) { // Algorithm 2: Temporal Knowledge Weighting
        val oldTrees = forest.filter( t => t.age > 1 / param.gamma)
        if (oldTrees.size > 0) {
          val t = oldTrees( Rand.nextInt(oldTrees.size) )
          if (t.oobe > Rand.nextDouble) t.reset
        }
      }
    }

    // mean Tree Stats
    def meanTreeSize = forest.map{_.tree.size}.sum / forest.size.toDouble
    def meanNumLeaves = forest.map{_.tree.numLeaves}.sum / forest.size.toDouble
    def meanMaxDepth = forest.map{_.tree.maxDepth}.sum / forest.size.toDouble

    // sd Tree Stats
    def sdTreeSize = sd(forest.map{_.tree.size}.toVector)
    def sdNumLeaves = sd(forest.map{_.tree.numLeaves}.toVector)
    def sdMaxDepth = sd(forest.map{_.tree.maxDepth}.toVector)

    /** Computes standard deviation of vector xs */
    protected def sd(xs: Vector[Int]) = {
      val n = xs.size.toDouble
      val mean = xs.sum / n
      scala.math.sqrt( xs.map(x => (x-mean) * (x-mean) ).sum / (n-1) )
    }
  }
}
