package ORF
/** ORF.Classification: Online Random Forest - Classification version */
object Classification {
  import ORF.Template._
  private val Rand = new scala.util.Random


  /** ORTree: Online Random Tree
   *  @constructor Initialize with parameter (param, see Param), and range of X (xrng)
   *  @param param parameter settings for online random tree. see class Param.
   *  @param xrng range for each column of X matrix. ( you can use Tools.dataRange(X) to get xrng )
   */
  object Classify { def apply(param: Param) = new Classify(new ClsElem(-1,0,0,param),param) }
  class Classify(override val elem: ClsElem, param: Param) extends ORTree (elem,param){ 
    def newElem: ClsElem = new ClsElem(-1,0,0,param)

    def loss(suff: Any): Double = suff match {
      case s: ClsSuffStats => {
        val n = s.counts.sum.toDouble + param.numClasses
        (s.counts map { x => val p = x / n; -p * scala.math.log(p) }).sum
      }
      case _ => 0.0
    }

    def updateOOBE(x: Vector[Double], y: Double) = {
      val pred = predict(x).toInt
      _oobe._2(y.toInt) += 1
      if (pred == y.toInt) _oobe._1(y.toInt) += 1
    }
    private val _oobe = (Array.fill(param.numClasses)(0), Array.fill(param.numClasses)(0))
    //def reset: Unit = {}
  }
}
