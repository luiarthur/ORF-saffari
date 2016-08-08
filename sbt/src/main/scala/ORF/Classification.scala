package ORF {
/** ORF.Classification: Online Random Forest - Classification version */
  import ORF.Template._
  import ORF.Tools.Param

  /** Classification on-line random tree
   *  @constructor Initialize with parameter (param, see Param).
   *  @param param parameter settings for online random tree. see class Param.
   */
  object ClassificationTree { def apply(param: Param) = new ClassificationTree(new ClsElem(-1,0,0,param),param) }
  class  ClassificationTree (override val elem: ClsElem, param: Param) extends ORTree (elem,param) { 

    // private field
    private var _oobe = (Array.fill(param.numClasses)(0), Array.fill(param.numClasses)(0))

    // protected methods
    protected def reset: Unit = {
      _oobe = ( Array(),Array() )
      _age = 0
      _tree = Tree( newElem )
    }
    protected def newElem: ClsElem = new ClsElem(-1,0,0,param)
    protected def updateOOBE(x: Vector[Double], y: Double) = {
      val pred = predict(x).toInt
      _oobe._2(y.toInt) += 1
      if (pred == y.toInt) _oobe._1(y.toInt) += 1
    }

  }
}
