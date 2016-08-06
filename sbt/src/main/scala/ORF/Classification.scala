package ORF
/** ORF.Classification: Online Random Forest - Classification version */
object Classification {
  import ORF.Template._
  private val Rand = new scala.util.Random

  /** Parameters for the OR-Tree, OR-Foest
   *  @constructor create a set of parameters for ORT / ORF
   *  @param numClasses  number of classes in response. e.g. if numClasses == 3, then the responses should be one of {0,1,2}
   *  @param minSamples  the minimum number of samples a node needs to see before it is permitted to split
   *  @param minGain  the minimum gain (based on metric) that a split needs to achieve before split can occur
   *  @param gamma  the temporal weighting learning rate (>0). if age of tree > 1/gamma, the tree is thrown away. (default=0)
   *  @param numTests  the number of tests (split dimension, location) each node does. (default=10)
   *  @param lam  the mean parameter in the poisson distribution. Should be set to 1 (default=1)
   *  @param metric  the metric that determines node purity ("entropy" or "gini"). (default="entropy")
   */

  /** ORTree: Online Random Tree
   *  @constructor Initialize with parameter (param, see Param), and range of X (xrng)
   *  @param param parameter settings for online random tree. see class Param.
   *  @param xrng range for each column of X matrix. ( you can use Tools.dataRange(X) to get xrng )
   */
  // Now work on this mess...
  case class Classify(param: Param, xrng: Vector[(Double,Double)]) extends {

    def reset = { 
      _tree = Tree( Info() )
      _age = 0
      for (d <- 0 until numClasses) {
        _oobe._1(d) = 0
        _oobe._2(d) = 0
      }
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
    
    private override def loss(c: Array[Int], metric: String = param.metric) = {
      val n = c.sum.toDouble + numClasses
      (c map { x => 
        val p = x / n
        if (metric == "gini") p * (1-p) else -p * scala.math.log(p)
      }).sum
    }

    private def gains(info: Info) = {
      val tests = info.tests
      tests map { test =>
        val cL = test.cLeft
        val nL = cL.sum + numClasses
        val cR = test.cRight
        val nR = cR.sum + numClasses
        val n = (nL + nR).toDouble
        val g = loss(info.c) - (nL/n) * loss(cL) - (nR/n) * loss(cR)
        if (g < 0) 0 else g
      }
    }

    /** Info holds the information within the nodes of the tree */
    case class Info(var splitDim: Int = -1, var splitLoc: Double = 0.0) {
      private var _numSamplesSeen = 0
      def numSamplesSeen = _numSamplesSeen
      var c = Array.fill(numClasses)(1)
      def numSamples = c.sum

      case class Test(dim: Int, loc: Double, cLeft: Array[Int], cRight: Array[Int])

      private var _tests = {
        def runif(rng: (Double,Double)) = Rand.nextDouble * (rng._2-rng._1) + rng._1
        def gentest = {
          val dim = Rand.nextInt(dimX)
          val loc = runif(xrng(dim))
          val cLeft = Array.fill(numClasses)(1)
          val cRight = Array.fill(numClasses)(1)
          Test(dim,loc,cLeft,cRight)
        }
        Array.range(0, numTests) map {s => gentest}
      }
      def tests = _tests

      // ToDo: ??? Gini importance: I = Gini - Gini_splitLeft - Gini_splitRight
      def reset = {
        c = Array()
        _tests = Array()
      }

      def update(x: Vector[Double], y: Int) = {
        c(y) += 1
        _numSamplesSeen = _numSamplesSeen + 1
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
  //    val predList = preds.groupBy(identity).toList
  //    predList.maxBy(_._2.size)._1
  //    /* Alternatively:
  //    val dens = forest.map(tree => tree.density(x))
  //    val inds = Vector.range(0,dens.head.size)
  //    val densMean = inds.map( i => dens.map(d => d(i)).sum / dens.size )
  //    val out = densMean.zipWithIndex.maxBy(_._1)._2
  //    out
  //    */
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

  //  /** Produces a confusion matrix based on a test set xs (input) and ys (response) */
  //  def confusion(xs: Vector[Vector[Double]], ys: Vector[Int]) = {
  //    assert(xs.size == ys.size, "Error: xs and ys need to have same length")
  //    val numClasses = param.numClasses
  //    val preds = xs.map(x => predict(x))
  //    val conf = Array.fill(numClasses)( Array.fill(numClasses)(0) )
  //    for ( (y,pred) <- ys zip preds) conf(y)(pred) += 1
  //    conf
  //  }

  //  /** prints the confusion matrix (conf: output from confusion(xs, ys)) */
  //  def printConfusion(conf: Array[Array[Int]]) = {
  //    println("Confusion Matrix:")
  //    print("y\\pred\t")
  //    (0 until param.numClasses).foreach( i => print(i + "\t") )
  //    println("\n")
  //    var r = 0
  //    conf.foreach{ row => 
  //      print(r + "\t")
  //      r = r + 1
  //      row.foreach(c => print(c + "\t"))
  //      println("\n")
  //    }
  //  }

  //  /** Returns prediction accuracy based on test input (xs) and test response (ys) */
  //  def predAccuracy(xs: Vector[Vector[Double]], ys: Vector[Int]) = {
  //    assert(xs.size == ys.size, "Error: xs and ys need to have same length")
  //    val pt = (xs zip ys) map {z => predict(z._1) == z._2}
  //    pt.map(predEqualTruth => if (predEqualTruth) 1 else 0).sum / pt.size.toDouble
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
  //    val n = ys.size
  //    val numClasses = param.numClasses
  //    val inds = Vector.range(0,n)
  //    val conf = Array.fill(numClasses)( Array.fill(numClasses)(0) )
  //    for (i <- inds) {
  //      val orf = ORForest(param,xrng,par=par)
  //      val indsShuf = Rand.shuffle(0 to n-1) // important
  //      val trainInds = indsShuf.filter(_!=i)
  //      trainInds.foreach{ i => orf.update(xs(i),ys(i).toInt) }
  //      val pred = orf.predict(xs(i))
  //      val curr = conf(ys(i))(pred)
  //      conf(ys(i))(pred) = curr + 1
  //    }
  //    conf
  //  }
  //}
}
