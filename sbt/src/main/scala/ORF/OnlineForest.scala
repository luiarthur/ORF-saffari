package ORF {
/** ORF.Classification: Online Random Forest - Classification version */
  import ORF.Template._
  import ORF.Tools.Param

  object ClsForest { 
    def apply(param: Param, numTrees: Int = 100, par: Boolean = false) = new ClsForest(param,numTrees,par) 
  }
  class ClsForest (param: Param, numTrees: Int = 100, par: Boolean = false) extends ORForest (param,numTrees,par) { 
    def newORTree = ORF.ClsTree(param)
  }

  object RegForest { 
    def apply(param: Param, numTrees: Int = 100, par: Boolean = false) = new RegForest(param,numTrees,par) 
  }
  class RegForest (param: Param, numTrees: Int = 100, par: Boolean = false) extends ORForest (param,numTrees,par) { 
    def newORTree = ORF.RegTree(param)
  }

}
