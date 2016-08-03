package ORF
/** ORF.Regression: Online Random Forest - Regression version */
object Regression {
  import ORF.Tree
  private val Rand = new scala.util.Random

  /** Parameters for the OR-Tree, OR-Foest
   *  @constructor create a set of parameters for ORT / ORF
   *  @param minSamples  the minimum number of samples a node needs to see before it is permitted to split
   *  @param minGain  the minimum gain (based on metric) that a split needs to achieve before split can occur
   *  @param gamma  the temporal weighting learning rate (>0). if age of tree > 1/gamma, the tree is thrown away. (default=0)
   *  @param numTests  the number of tests (split dimension, location) each node does. (default=10)
   *  @param lam  the mean parameter in the poisson distribution. Should be set to 1 (default=1)
   */
  case class Param(numClasses: Int, minSamples: Int, minGain: Double, gamma: Double = 0, numTests: Int = 10, lam: Double=1) {
    assert(lam <= 5, "Current implementation (Knuth) only supports small lam. lam=1 is suitable for most bootstrapping cases.")
  }

  /** ORTree: Online Random Tree
   *  @constructor Initialize with parameter (param, see Param), and range of X (xrng)
   *  @param param parameter settings for online random tree. see class Param.
   *  @param xrng range for each column of X matrix. ( you can use Tools.dataRange(X) to get xrng )
   */
  case class ORTree (param: Param, xrng: Vector[(Double,Double)]) {

    val numTests = if (param.numTests == 0) scala.math.sqrt(xrng.size).toInt else param.numTests
    val minSamples = param.minSamples
    val minGain = param.minGain
    val lam = param.lam


    /** age of tree = the number of observations a tree has seen. Only used if gamma > 0. Old trees are thrown & re-grown.*/
    def age = _age
    private var _age = 0

    /** dimension of X matrix */
    private val dimX = xrng.size

    /** out of bag error (oobe = rmse) of online random tree*/
    def oobe = {
      val ybar = oobSumY / oobNum
      scala.math.sqrt( oobSumSqY / oobNum - ybar*ybar  )
    }
    private var oobNum = 0L
    private var oobSumY, oobSumSqY = 0.0

    /** the online random tree*/
    def tree = _tree
    private var _tree = Tree( Info() ) // Online Tree

    /** reset the online random tree (_tree), reset tree age to 0, reset oobe to 0. Only if gamma > 0. */
    def reset = { 
      _tree = Tree( Info() )
      _age = 0
      oobNum = 0
      oobSumY = 0
      oobSumSqY = 0
    }

    private def findLeaf(x: Vector[Double], tree: Tree[Info]): Tree[Info] = {
      if (tree.isLeaf) tree else {
        val (dim,loc) = (tree.elem.splitDim, tree.elem.splitLoc)
        if ( x(dim) > loc ) findLeaf(x, tree.right) else findLeaf(x, tree.left)
      }
    }

    /** Online Random Tree Prediction based on (x) */
    def predict(x: Vector[Double]) = findLeaf(x,tree).elem.pred
    /** Online Random Tree Prediction based on (x) */
    def density(x: Vector[Double]) = findLeaf(x,tree).elem.dens

    /** sample from poisson. lam should ideally always be 1. */
    private def poisson(lam: Double) = {
      val l = scala.math.exp(-lam)
      def loop(k: Int, p: Double): Int = if (p > l) loop(k+1, p * Rand.nextDouble) else k - 1
      loop(0,1)
    }
    
    /** update _tree based on new observations x, y */
    def update(x: Vector[Double], y: Int) = {
      val k = poisson(lam)
      if (k > 0) {
        for (u <- 1 to k) {
          _age = _age + 1
          val j = findLeaf(x,_tree)
          j.elem.update(x,y)
          if (j.elem.numSamplesSeen > minSamples) {
            val g = gains(j.elem)
            if ( g.exists(_ > minGain) ) {
              val bestTest = g.zip(j.elem.tests).maxBy(_._1)._2
              // create Left, Right children
              j.left = Tree( Info() )
              j.right = Tree( Info() )
              j.elem.splitDim = bestTest.dim
              j.elem.splitLoc = bestTest.loc
              j.left.elem.c = bestTest.cLeft
              j.right.elem.c = bestTest.cRight
              j.elem.reset
            }
          }
        }
      } else { // k > 0
        // estimate OOBE: Used for Temporal Knowledge Weighting
        val pred = predict(x)
        _oobe = ???
      }
    }

    private def loss(c: Array[Int]) = {
      ???
    }

    private def gains(info: Info) = {
      val tests = info.tests
      tests map { test =>
        ???
      }
    }

    /** Info holds the information within the nodes of the tree */
    case class Info(var splitDim: Int = -1, var splitLoc: Double = 0.0) {
      private var _numSamplesSeen = 0
      def numSamplesSeen = _numSamplesSeen
      var sumC = 0.0
      var ssC = 0.0

      case class Test(dim: Int, loc: Double) {
          var sumL = 0.0
          var ssL = 0.0
          var nL = 0

          var sumR = 0.0
          var ssR = 0.0
          var nR = 0
      }

      private var _tests = {
        def runif(rng: (Double,Double)) = Rand.nextDouble * (rng._2-rng._1) + rng._1
        def gentest = {
          val dim = Rand.nextInt(dimX)
          val loc = runif(xrng(dim))
          Test(dim,loc)
        }
        Array.range(0, numTests) map {s => gentest}
      }
      def tests = _tests

      def reset = {
        _tests = Array()
      }

      def update(x: Vector[Double], y: Int) = {
        c(y) += 1
        _numSamplesSeen += 1
        for (test <- tests) {
          val dim = test.dim
          val loc = test.loc
          if (x(dim) < loc) {
            test.cLeft(y) += 1
          } else {
            test.cRight(y) += 1
          }
        }
      }
      
      def pred = c.zipWithIndex.maxBy(_._1)._2
      def dens = c.map { cc => cc / (numSamples.toDouble + numClasses) }

      override def toString = if (splitDim == -1) pred.toString else "X" + (splitDim+1) + " < " + (splitLoc * 100).round / 100.0
    } // end of case class Info
  } // end of case class ORT

  ///** ORForest: Online Random Forest for Classification.
  // *  @constructor creates online random forest object
  // *  @param param parameter settings for random forest
  // *  @param xrng range for each column of X matrix. ( you can use Tools.dataRange(X) to get xrng )
  // *  @param numTrees number of trees in forest
  // *  @param par if true, trees in forest are updated in parallel. Otherwise, trees in forest are updated sequentially.
  // */
  //case class ORForest(param: Param, xrng: Vector[(Double,Double)], numTrees: Int = 100, par: Boolean = false) {

  //  val gamma = param.gamma
  //  val lam = param.lam

  //  private var _forest = {
  //    val f = Vector.range(1,numTrees) map { i => 
  //      val tree = ORTree(param,xrng)
  //      tree
  //    }
  //    if (par) f.par else f
  //  }
  //  def forest = _forest

  //  /** predict / classify based on input x */
  //  def predict(x: Vector[Double]) = {
  //    val preds = forest.map(tree => tree.predict(x)) 
  //    ???
  //  }

  //  /** update the random forest based on new observations x, y */
  //  def update(x: Vector[Double], y: Int) = {
  //    _forest.foreach( _.update(x,y) )
  //    if (gamma > 0) { // Algorithm 2: Temporal Knowledge Weighting
  //      val oldTrees = forest.filter( t => t.age > 1 / gamma)
  //      if (oldTrees.size > 0) {
  //        val t = oldTrees( Rand.nextInt(oldTrees.size) )
  //        if (t.oobe > Rand.nextDouble) t.reset
  //      }
  //    }
  //  }

  //  /** Returns prediction accuracy based on test input (xs) and test response (ys) */
  //  def rmse(xs: Vector[Vector[Double]], ys: Vector[Int]) = {
  //    assert(xs.size == ys.size, "Error: xs and ys need to have same length")
  //    ???
  //  }

  //  // mean Tree Stats
  //  def meanTreeSize = forest.map{_.tree.size}.sum / forest.size.toDouble
  //  def meanNumLeaves = forest.map{_.tree.numLeaves}.sum / forest.size.toDouble
  //  def meanMaxDepth = forest.map{_.tree.maxDepth}.sum / forest.size.toDouble

  //  // sd Tree Stats
  //  def sdTreeSize = sd(forest.map{_.tree.size}.toVector)
  //  def sdNumLeaves = sd(forest.map{_.tree.numLeaves}.toVector)
  //  def sdMaxDepth = sd(forest.map{_.tree.maxDepth}.toVector)

  //  /** Computes standard deviation of vector xs */
  //  private def sd(xs: Vector[Int]) = {
  //    val n = xs.size.toDouble
  //    val mean = xs.sum / n
  //    scala.math.sqrt( xs.map(x => (x-mean) * (x-mean) ).sum / (n-1) )
  //  }

  //  /** Leave one out Cross Validation. Probably not practical in streaming set-up. But useful for testing*/
  //  def leaveOneOutCV(xs: Vector[Vector[Double]], ys: Vector[Int], par: Boolean = false) = {
  //    assert(xs.size == ys.size, "Error: xs and ys need to have same length")
  //    ???
  //  }
  //}
}
