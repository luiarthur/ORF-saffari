object ORF {
  import scala.math.{log,exp,sqrt}
  private val Rand = scala.util.Random

  // Tools:
  def dataRange(X: Vector[Vector[Double]]) = 
    Vector.range(0,X(0).size) map {j => 
      val colj = X map {x => x(j)}
      (colj.max, colj.min)
    }
  def dataClasses(y: Vector[Double]) = y.toSet.size
  def normalize(X: Vector[Vector[Double]]) = {
    val rng = dataRange(X)
    val n = X.size
    val k = X(0).size
    Vector.range(0,n) map { i =>
      Vector.range(0,k) map { j => 
        ( X(i)(j) - rng(j)._1 ) / ( rng(j)._2 - rng(j)._1)
      }
    }
  }
  private def poisson(lam: Double) = {
    val l = exp(-lam)
    def loop(k: Int, p: Double): Int = if (p > l) loop(k+1, p * Rand.nextDouble) else k - 1
    loop(0,1)
  }

  // Mutable Left, Right Tree
  case class Tree[T](elem: T, var left: Tree[T] = null, var right: Tree[T] = null) {
    def isLeaf = (left,right) match {case (null,null) => true; case _ => false}
    def size: Int = if (isLeaf) 1 else left.size + right.size + 1
    def numLeaves: Int = if (isLeaf) 1 else left.numLeaves + right.numLeaves
    def inOrder: List[T] = if (isLeaf) List(this.elem) else 
      left.inOrder ::: List(this.elem) ::: right.inOrder
    def preOrder: List[T] = if (isLeaf) List(this.elem) else 
      List(this.elem) ::: left.inOrder ::: right.inOrder

    private def pretty(spacing: Int = 3): Vector[String] = {
      def rep(n: Int, s: String=" ") = List.fill(n)(s).mkString
      def paste(l: Vector[String], r: Vector[String]) = {
        def elongate(vs: Vector[String]) = {
          val maxCol = vs.map(_.size).max
          vs.map( s => s + rep(maxCol - s.size) )
        }
        val maxRow = List(l,r).map(_.size).max
        val newlr = Vector(l,r).map(x => x ++ Vector.fill(maxRow-x.size)("")).map(elongate(_))
        val out = for (i <- (0 until maxRow)) yield newlr(0)(i) + newlr(1)(i)
        out.toVector
      }
      val ps = elem.toString
      val List(ls,rs) = List(left,right).map(x => if (x.isLeaf) Vector(x.elem.toString) else x.pretty(spacing))
      val posL = ls(0).indexOf(left.elem.toString)
      val posR = rs(0).indexOf(right.elem.toString)
      val top = rep(posL) + rep(spacing+ls(0).size-posL,"_") + ps + rep(spacing+posR,"_") + rep(rs(0).size-posR)
      val bottom = List(ls, Vector(rep(spacing*2 + ps.size)), rs).reduce(paste)
      Vector(top) ++ bottom
    }
    def treeString = if (isLeaf) "Leaf(" + elem.toString + ")" else "\n" + pretty(spacing=1).mkString("\n") + "\n"
    def draw = println(treeString)
  }

  case class Param(numClasses: Int, minSamples: Int, minGain: Double, 
                   gamma: Int = 0, numTests: Int = 10, lam: Double=1) {
    assert(lam <= 3, "Current implementation only supports lam <= 3. lam=1 is suitable for most bootstrapping cases.")
  }

  case class OT (param: Param, xRange: Vector[(Double,Double)]) { // for classification

    private var _age = 0
    def age = _age
    val lam = param.lam
    val numTests = if (param.numTests == 0) sqrt(xRange.size).toInt else param.numTests
    val numClasses = param.numClasses
    val minSamples = param.minSamples
    val minGain = param.minGain
    val dimX = xRange.size
    private val _oobe = (Array.fill(numClasses)(0), Array.fill(numClasses)(0)) // (# correct, # Total)
    def oobe = { (_oobe._1 zip _oobe._2) map {z => if (z._2 == 0) 0 else 1 - z._1 / z._2.toDouble} }.sum / numClasses

    private var _tree = Tree( Info() ) // Online Tree
    def tree = _tree
    def reset = { 
      _tree = Tree( Info() )
    }

    private def findLeaf(x: Vector[Double], tree: Tree[Info]): Tree[Info] = {
      if (tree.isLeaf) tree else {
        val (dim,loc) = (tree.elem.splitDim, tree.elem.splitLoc)
        if ( x(dim) > loc ) findLeaf(x, tree.right) else findLeaf(x, tree.left)
      }
    }

    def predict(x: Vector[Double]) = findLeaf(x,tree).elem.pred
    def density(x: Vector[Double]) = findLeaf(x,tree).elem.dens
    
    def update(x: Vector[Double], y: Int) = { // Updates _tree
      val k = poisson(1)
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
        _oobe._2(y) += 1
        if (pred == y) _oobe._1(y) += 1
      }
    }

    private def loss(c: Array[Int], metric: String = "entropy") = {
      val n = c.sum.toDouble + numClasses
      (c map { x => 
        val p = x / n
        if (metric == "gini") p * (1-p) else -p * log(p)
      }).sum
    }

    private def gains(info: Info) = {
      val tests = info.tests
      val n = info.numSamples.toDouble + numClasses
      tests map { test =>
        val cL = test.cLeft
        val nL = cL.sum + numClasses
        val cR = test.cRight
        val nR = cR.sum + numClasses
        val g = loss(info.c) - (nL/n) * loss(cL) - (nR/n) * loss(cR)
        if (g < 0) 0 else g
      }
    }

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
          val loc = runif(xRange(dim))
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

      override def toString(): String = 
        if (splitDim == -1) pred.toString else "X" + (splitDim+1) + " < " + (splitLoc * 100).round / 100.0
    } // end of case class Info
  } // end of case class OT

  case class Forest(param: Param, rng: Vector[(Double,Double)], numTrees: Int = 100, par: Boolean = false) {
    val gamma = param.gamma
    val lam = param.lam
    private var _forest = {
      val f = Vector.range(1,numTrees) map { i => 
        val tree = OT(param,rng)
        tree
      }
      if (par) f.par else f
    }
    def forest = _forest
    def predict(x: Vector[Double]) = {
      val preds = forest.map(tree => tree.predict(x)) 
      val predList = preds.groupBy(identity).toList
      predList.maxBy(_._2.size)._1
      /* Alternatively:
      val dens = forest.map(tree => tree.density(x))
      val inds = Vector.range(0,dens.head.size)
      val densMean = inds.map( i => dens.map(d => d(i)).sum / dens.size )
      val out = densMean.zipWithIndex.maxBy(_._1)._2
      out
      */
    }
    def update(x: Vector[Double], y: Int) = {
      _forest.foreach( _.update(x,y) )
      if (gamma > 0) { // Algorithm 2: Temporal Knowledge Weighting
        val oldTrees = forest.filter( t => t.age > 1 / gamma)
        if (oldTrees.size > 0) {
          val t = oldTrees( Rand.nextInt(oldTrees.size) )
          if (t.oobe > Rand.nextDouble) t.reset
        }
      }
    }
    def confusion(xs: Vector[Vector[Double]], ys: Vector[Int]) = {
      assert(xs.size == ys.size, "Error: xs and ys need to have same length")
      val numClasses = param.numClasses
      val preds = xs.map(x => predict(x))
      val conf = Array.fill(numClasses)( Array.fill(numClasses)(0) )
      for ( (y,pred) <- ys zip preds) conf(y)(pred) += 1
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
    def predAccuracy(xs: Vector[Vector[Double]], ys: Vector[Int]) = {
      assert(xs.size == ys.size, "Error: xs and ys need to have same length")
      val pt = (xs zip ys) map {z => predict(z._1) == z._2}
      pt.map(predEqualTruth => if (predEqualTruth) 1 else 0).sum / pt.size.toDouble
    }
    def meanTreeSize = forest.map{ot => ot.tree.size}.sum / forest.size.toDouble
    def meanNumLeaves = forest.map{ot => ot.tree.numLeaves}.sum / forest.size.toDouble

    def leaveOneOutCV(xs: Vector[Vector[Double]], ys: Vector[Int], par: Boolean = false) = { // for convenience
      assert(xs.size == ys.size, "Error: xs and ys need to have same length")
      val n = ys.size
      val numClasses = param.numClasses
      val inds = Vector.range(0,n)
      val conf = Array.fill(numClasses)( Array.fill(numClasses)(0) )
      for (i <- inds) {
        val orf = Forest(param,rng,par=par)
        val indsShuf = Rand.shuffle(0 to n-1) // important
        val trainInds = indsShuf.filter(_!=i)
        trainInds.foreach{ i => orf.update(xs(i),ys(i).toInt) }
        val pred = orf.predict(xs(i))
        val curr = conf(ys(i))(pred)
        conf(ys(i))(pred) = curr + 1
      }
      conf
    }
  }
}
