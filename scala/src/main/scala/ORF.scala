object ORF {
  // class Tree: 
  case class Tree[T](elem: T, left: Tree[T] = null, right: Tree[T] = null) {
    def isLeaf = (left,right) match {case (null,null) => true; case _ => false}
    def nodes: List[Tree[T]] = if (isLeaf) List(this) else left.nodes ::: right.nodes ::: List(this)

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

  case class Info (numClass: Int) {
    var splitDim: Int = 0
    var splitLoc: Double  = 0.0
    var p: Vector[Int] = Vector.fill(numClass)(0)
    var pLeft: Vector[Int] = Vector.fill(numClass)(0)
    var pRight: Vector[Int] = Vector.fill(numClass)(0)
  }

  type Param = Map[String,Double]
  //val defaultParam = Map[String,Double]("lam" -> 1, "numClass" -> 5, "alpha" -> 50, "beta" -> .1, "numTests" -> 10)
  class OT (param: Param) {
    import breeze.stats.distributions.Poisson

    val numClass = param("numClass").toInt
    private var _tree = Tree( Info(numClass) )
    def tree = _tree

    def findLeaf(x: Vector[Double]): Tree[Info] = {
      ???
    }

    def predict(x: Vector[Double]) = ???
    def update(x: Vector[Double], y: Int) = {
      val k = Poisson(param("lam")).draw
      if (k > 0) {
        for (u <- 1 to k) {

        }
      } else {
        // estimate OOBE
      }
      ???
    }
  }

  // Think about the design
  /*
  class Forest[T](x: Vector[Double], y: T, numClass:Int, lam: Double = 1, alpha: Int = 1, beta: Double = .1, numTests: Int = 10,
    numTrees: Int = 100) {
    import breeze.stats.distributions.Poisson

    val response = y match { case i: Int => "Int"; case d: Double => "Double"; case _ => "InvalidResponseType" }
    val forest = List.fill(numTrees)( new Tree(Info) ).par

    def predict(x: Vector[Double]) = ???
    def update(x: Vector[Double], y: T) = {
      forest map { t =>
        val k = Poisson(lam).draw
      }
    }
    def labelProps(y: Vector[Int], nc: Int = numClass) {
      val labels = Vector.range(0,nc)
      val n = y.size
      labels map { k  => y.count(_ == k) / n.toDouble }
    }
    def gini(y: Vector[Int], nc: Int = numClass) = { // smaller is better => purer node
      val labels = Vector.range(0,nc)
      val n = y.size
      (labels map { k  =>
        val p = y.count(_ == k) / n.toDouble
        p * (1-p)
      }).sum
    }
  }
  */
}

/*
   import ORF._
   val x = Tree(1,Tree(202),Tree(303))
   x.isLeaf
   x.left.isLeaf
   x.draw
   x.left.draw
   val f = new Forest(Vector(1,2,3), 0)
   f.response
   val f = new Forest(Vector(1,2,3), .0)
   f.response
   List.fill(10)(Tree(1)).par
 */
