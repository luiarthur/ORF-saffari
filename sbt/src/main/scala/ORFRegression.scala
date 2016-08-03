object ORFRegression {
  import scala.math.{log,exp,sqrt}
  private val Rand = scala.util.Random

  // Tools:
  def dataRange(X: Vector[Vector[Double]]) = 
    Vector.range(0,X(0).size) map {j => 
      val colj = X map {x => x(j)}
      (colj.max, colj.min)
    }
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
    private def md(s: Int): Int = if (isLeaf) s else scala.math.max(left.md(s+1),right.md(s+1))
    def maxDepth = md(1)

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

  case class Param(minSamples: Int, minGain: Double, gamma: Double = 0, numTests: Int = 10, lam: Double=1) {
    assert(lam <= 10, "Current implementation only supports lam <= 10. lam=1 is suitable for most bootstrapping cases.")
  }

  case class OT (param: Param, xRange: Vector[(Double,Double)]) { // for classification

    private var _age = 0
    def age = _age
    val lam = param.lam
    val numTests = if (param.numTests == 0) sqrt(xRange.size).toInt else param.numTests
    val minSamples = param.minSamples
    val minGain = param.minGain
    val dimX = xRange.size
    private var _oobe = (0.0, 0) // (# sum, # n)
    def oobe = sqrt(_oobe._1 / _oobe._2)

    private var _tree = Tree( Info() ) // Online Tree
    def tree = _tree
    def reset = { 
      _tree = Tree( Info() )
      _age = 0
      _oobe._1 = 0
      _oobe._2 = 0
    }

    private def findLeaf(x: Vector[Double], tree: Tree[Info]): Tree[Info] = {
      if (tree.isLeaf) tree else {
        val (dim,loc) = (tree.elem.splitDim, tree.elem.splitLoc)
        if ( x(dim) > loc ) findLeaf(x, tree.right) else findLeaf(x, tree.left)
      }
    }

    def predict(x: Vector[Double]) = findLeaf(x,tree).elem.pred
    
    def update(x: Vector[Double], y: Int) = { // Updates _tree
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
              //j.left.elem.c = bestTest.cLeft
              //j.right.elem.c = bestTest.cRight
              //j.elem.reset
              ???
            }
          }
        }
      } else { // k > 0
        // estimate OOBE: Used for Temporal Knowledge Weighting
        //val pred = predict(x)
        //_oobe._2(y) += 1
        //if (pred == y) _oobe._1(y) += 1
        ???
      }
    }

    // STOPPED HERE: 
    private def loss(c,y) = ???

    private def gains(info: Info) = ???

    case class Info(var splitDim: Int = -1, var splitLoc: Double = 0.0) {
      ???
      private var _numSamplesSeen = 0
      def numSamplesSeen = _numSamplesSeen
      var sumC = 0.0

      case class Test(dim: Int, loc: Double, var sumL: Double, var sumRight: Double, var nL: Int, var nR: Int)

      private var _tests = {
        def runif(rng: (Double,Double)) = Rand.nextDouble * (rng._2-rng._1) + rng._1
        def gentest = {
          val dim = Rand.nextInt(dimX)
          val loc = runif(xRange(dim))
          val sumL = 0.0
          val sumR = 0.0
          Test(dim,loc,sumL,sumR,0,0)
        }
        Array.range(0, numTests) map {s => gentest}
      }
      def tests = _tests

      def reset = {
        _tests = Array()
      }

      def update(x: Vector[Double], y: Double) = {
        sumC += y
        _numSamplesSeen = _numSamplesSeen + 1
        for (test <- tests) {
          val dim = test.dim
          val loc = test.loc
          if (x(dim) < loc) {
            test.sumLeft += y
            test.nL += 1
          } else {
            test.sumRight += y
            test.nR += 1
          }
        }
      }
      
      def pred = sumC / numSamplesSeen

      override def toString = if (splitDim == -1) pred.toString else "X" + (splitDim+1) + " < " + (splitLoc * 100).round / 100.0
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
      preds.sum / preds.size
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
    def rmse(xs: Vector[Vector[Double]], ys: Vector[Int]) = {
      assert(xs.size == ys.size, "Error: xs and ys need to have same length")
      val pt = (xs zip ys) map {z => (z._1-z._2) * (z._1-z._2)}
      sqrt( pt.sum / xs.size )
    }
    def meanTreeSize = forest.map{ot => ot.tree.size}.sum / forest.size.toDouble
    def meanNumLeaves = forest.map{ot => ot.tree.numLeaves}.sum / forest.size.toDouble
    def meanMaxDepth = forest.map{ot => ot.tree.maxDepth}.sum / forest.size.toDouble
    private def sd(xs: Vector[Int]) = {
      val n = xs.size.toDouble
      val mean = xs.sum / n
      sqrt( xs.map(x => (x-mean) * (x-mean) ).sum / (n-1) )
    }
  }
}
