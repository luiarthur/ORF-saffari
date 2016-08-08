package ORF {
/** ORF.Classification: Online Random Forest - Classification version */
  import ORF.Template._
  import ORF.Tools.Param

  class ClsForest (param: Param, numTrees: Int = 100, par: Boolean = false) extends ORForest (param,numTrees,par) { 
    def newORTree = ORF.ClsTree(param)
  }
  class RegForest (param: Param, numTrees: Int = 100, par: Boolean = false) extends ORForest (param,numTrees,par) { 
    def newORTree = ORF.RegTree(param)
  }

}
