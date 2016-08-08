package ORF {
/** ORF.Classification: Online Random Forest - Classification version */
  import ORF.Template._
  import ORF.Tools.Param


  /** Classification on-line random tree
   *  @constructor Initialize with parameter (param, see Param).
   *  @param param parameter settings for online random tree. see class Param.
   */
  object ClsTree { def apply(param: Param) = new ClsTree(new ClsElem(-1,0,0,param),param) }
  class  ClsTree (override val elem: ClsElem, param: Param) extends ORTree (elem,param) { 
    private var _oobe = (Array.fill(param.numClasses)(0), Array.fill(param.numClasses)(0)) // #correct, #wrong
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
    def oobe = { (_oobe._1 zip _oobe._2) map {z => if (z._2 == 0) 0 else 1 - z._1 / z._2.toDouble} }.sum / param.numClasses
  }

  /** Classification on-line random tree
   *  @constructor Initialize with parameter (param, see Param).
   *  @param param parameter settings for online random tree. see class Param.
   */
  object RegTree { def apply(param: Param) = new RegTree(new RegElem(-1,0,0,param),param) }
  class  RegTree (override val elem: RegElem, param: Param) extends ORTree (elem,param) { 
    private var _oobN = 0
    private var _oobSum = 0.0
    private var _oobSS = 0.0
    def oobe = {
      val ybar = _oobSum / _oobN.toDouble
      scala.math.sqrt((_oobSS - ybar*ybar) / _oobN.toDouble)
    }

    // protected methods
    protected def reset: Unit = {
      _age = 0
      _tree = Tree( newElem )
      _oobN = 0
      _oobSum = 0.0
      _oobSS = 0.0
    }
    protected def newElem: RegElem = new RegElem(-1,0,0,param)
    protected def updateOOBE(x: Vector[Double], y: Double) = {
      val pred = predict(x).toInt
      _oobN += 1
      _oobSum += y
      _oobSS += y*y
    }
  }

}
