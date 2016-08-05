package ORF
/** ORF.Classification: Online Random Forest - Classification version */
object ClassificationImmutable {
  import ORF.Tree
  import ORF.NodeElem._
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
  case class Param(numClasses: Int, minSamples: Int, minGain: Double, 
                   gamma: Double = 0, numTests: Int = 10, lam: Double=1,
                   metric: String = "entropy") {
    assert(lam <= 10, "Current implementation only supports lam <= 10. lam=1 is suitable for most bootstrapping cases.")
  }

  /** ORTree: Online Random Tree
   *  @constructor Initialize with parameter (param, see Param), and range of X (xrng)
   *  @param param parameter settings for online random tree. see class Param.
   *  @param xrng range for each column of X matrix. ( you can use Tools.dataRange(X) to get xrng )
   */
  object ORTree {
    def apply(e: Elem, param: Param, xrng: Vector[(Double,Double)], age: Int): ORTree = 
      new ORTree(e, null, null, param, xrng, age)
    def apply(e: Elem, l: ORTree, r: ORTree, param: Param, xrng: Vector[(Double,Double)], age: Int): ORTree = 
      new ORTree(e, l, r, param, xrng, age)
    def apply(param: Param, xrng: Vector[(Double,Double)]): ORTree = {
      val c = Vector.fill(param.numClasses)(0)
      val t = generateTests(param.numTests, param.numClasses, xrng)
      new ORTree(Elem(counts=c,tests=t), null, null, param, xrng, 0)
    }
  }
  class ORTree(override val elem: Elem = Elem(), override val left: ORTree, override val right: ORTree, 
                val param: Param, val xrng: Vector[(Double,Double)], val age: Int = 0) extends Tree[Elem](elem,left,right) {

    private def singleUpdate(x: Vector[Double], y: Int): ORTree = { // core of algorithm
      if (isLeaf) {
        val newAge = age + 1
        val updatedElem = elem.update(x,y)
        val g = gains(updatedElem)
        val newTree = if ( updatedElem.numSamplesSeen > param.minSamples && g.exists(_ > param.minGain) ) {
          val bestTest = g.zip(updatedElem.tests).maxBy(_._1)._2
          val l = ORTree(Elem(counts=bestTest.countsLeft, tests=generateTests(param.numTests,param.numClasses,xrng)),param,xrng,newAge)
          val r = ORTree(Elem(counts=bestTest.countsRight,tests=generateTests(param.numTests,param.numClasses,xrng)),param,xrng,newAge)
          ORTree(Elem(splitDim=bestTest.dim, splitLoc=bestTest.loc),l,r,param,xrng,newAge) 
        } else ORTree(updatedElem,param,xrng,newAge)
        newTree
      } else { // not a leaf
        val (dim,loc) = (elem.splitDim, elem.splitLoc)
        if (x(dim) < loc) ORTree(elem,left.singleUpdate(x,y),right,param,xrng,age) else 
          ORTree(elem,left,right.singleUpdate(x,y),param,xrng,age)
      }
    }

    def update(x: Vector[Double], y: Int) = {
      val k = poisson(param.lam)
      def loop(tree:ORTree, k: Int): ORTree = 
        if (k==0) {
          // compute oobe
          tree 
        } else loop( tree.singleUpdate(x,y), k-1)

      loop(ORTree(elem,left,right,param,xrng,age),k)
    }

    def predict(x: Vector[Double]) = findLeaf(x).elem.pred
    private def findLeaf(x: Vector[Double]): ORTree =
      if (isLeaf) this else {
        val (dim,loc) = (elem.splitDim, elem.splitLoc)
        if ( x(dim) > loc ) right.findLeaf(x) else left.findLeaf(x)
      }

    val dimX = xrng.size
    private def gains(elem: Elem) = {
      val nc = param.numClasses
      val tests = elem.tests
      tests map { test =>
        val cL = test.countsLeft
        val nL = cL.sum + nc
        val cR = test.countsRight
        val nR = cR.sum + nc
        val n = (nL + nR) + nc.toDouble
        val g = loss(elem.counts) - (nL/n) * loss(cL) - (nR/n) * loss(cR)
        if (g < 0) 0 else g
      }
    }
    private def loss(counts: Vector[Int]) = {
      val n = counts.sum.toDouble + param.numClasses.toDouble
      (counts map { x =>
        val p = x / n
        -p * scala.math.log(p) // entropy
      }).sum
    }
    private def poisson(lam: Double) = {
      val l = scala.math.exp(-lam)
      def loop(k: Int, p: Double): Int = if (p > l) loop(k+1, p * Rand.nextDouble) else k - 1
      loop(0,1)
    }
  } // end of ORTree


  case class ORForest(param: Param, xrng: Vector[(Double,Double)], numTrees: Int = 100) {

    private var _forest = List.fill(numTrees)(ORTree(param,xrng)).par
    def forest = _forest

    def update(x: Vector[Double], y: Int) = {
      val newForest =  _forest map { _.update(x,y) }
      _forest = newForest
    }

    def predict(x: Vector[Double]) = {
      assert(x.size == xrng.size, "Error in Forest.predict. x has the wrong size...")
      val preds = forest map { _.predict(x) }
      val predList = preds.groupBy(identity).toList
      predList.maxBy(_._2.size)._1
    }

    def predAccuracy(xs: Vector[Vector[Double]], ys: Vector[Int]) = {
      assert(xs.size == ys.size, "Error: xs and ys need to have same length")
      val pt = (xs zip ys) map {z => predict(z._1) == z._2}
      pt.map(predEqualTruth => if (predEqualTruth) 1 else 0).sum / pt.size.toDouble
    }

    def leaveOneOutCV(xs: Vector[Vector[Double]], ys: Vector[Int], par: Boolean = false) = {
      assert(xs.size == ys.size, "Error: xs and ys need to have same length")
      val n = ys.size
      val numClasses = param.numClasses
      val inds = Vector.range(0,n)
      val conf = Array.fill(numClasses)( Array.fill(numClasses)(0) )
      for (i <- inds) {
        val orf = ORForest(param,xrng)
        val indsShuf = Rand.shuffle(0 to n-1) // important
        val trainInds = indsShuf.filter(_!=i)
        trainInds.foreach{ i => orf.update(xs(i),ys(i).toInt) }
        val pred = orf.predict(xs(i))
        val curr = conf(ys(i))(pred)
        conf(ys(i))(pred) = curr + 1
      }
      conf
    }

    def printConfusion(conf: Array[Array[Int]]) = {
      println("Confusion Matrix:")
      print("y\\pred\t")
      (0 until param.numClasses).foreach( i => print(i + "\t") )
      println("\n")
      var r = 0
      conf.foreach{ row => 
        print(r + "\t")
        r = r + 1
        row.foreach(c => print(c + "\t"))
        println("\n")
      }
    }

  } // end of ORForest

}
