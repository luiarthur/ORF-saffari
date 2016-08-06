package ORF

//object Tree { def apply[T](elem: T): Tree[T] = Tree(elem, null, null) }
//case class Tree[T](val elem: T, private var _left: Tree[T], private var _right: Tree[T]) {
case class Tree[T](val elem: T, var left: Tree[T] = null, var right: Tree[T] = null) {
  //def left  = _left
  //def right = _right
  //def updateChildren(l: Tree[T], r: Tree[T]) = { _left = l; _right = r }
  /** true if the node is a leaf, false otherwise. */
  def isLeaf = (left,right) match {case (null,null) => true; case _ => false}

  /** returns number of nodes in tree */
  def size: Int = if (isLeaf) 1 else left.size + right.size + 1

  /** returns number of leaves in tree */
  def numLeaves: Int = if (isLeaf) 1 else left.numLeaves + right.numLeaves

  /** return elements in tree traversed in-order */
  def inOrder: List[T] = if (isLeaf) List(this.elem) else 
    left.inOrder ::: List(this.elem) ::: right.inOrder

  /** return elements in tree traversed pre-order */
  def preOrder: List[T] = if (isLeaf) List(this.elem) else 
    List(this.elem) ::: left.inOrder ::: right.inOrder

  /** return the maximum depth of the tree*/
  def maxDepth = md(1)
  private def md(s: Int): Int = if (isLeaf) s else scala.math.max(left.md(s+1),right.md(s+1))

  /** returns a pretty string for the tree*/
  def treeString = if (isLeaf) "Leaf(" + elem.toString + ")" else "\n" + pretty(spacing=1).mkString("\n") + "\n"

  /** prints the pretty tree. Just: println(treeString) */
  def draw = println(treeString)

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
}
