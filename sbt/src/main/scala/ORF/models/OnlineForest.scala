package ORF.models
/** ORF.Classification: Online Random Forest - Classification version */
import ORF.Template._

object ClsForest { 
  def apply(param: Param, numTrees: Int = 100, par: Boolean = false) = new ClsForest(param,numTrees,par) 
}
class ClsForest (param: Param, numTrees: Int = 100, par: Boolean = false) extends ORForest (param,numTrees,par) { 
  def newORTree = ClsTree(param)

  def predict(x: Vector[Double]) = {
    val preds = forest.map(tree => tree.predict(x)) 
    val predList = preds.groupBy(identity).toList
    predList.maxBy(_._2.size)._1
    // Alternatively:
    // val dens = forest.map(tree => tree.density(x))
    // val inds = Vector.range(0,dens.head.size)
    // val densMean = inds.map( i => dens.map(d => d(i)).sum / dens.size )
    // densMean.zipWithIndex.maxBy(_._1)._2
  }

  /** Returns prediction accuracy based on test input (xs) and test response (ys): Classification */
  def predAcc(xs: Vector[Vector[Double]], ys: Vector[Double]) = {
    assert(xs.size == ys.size, "Error: xs and ys need to have same length")
    val preds = xs map { predict(_) }
    val bools = {preds zip ys} map {z => if (z._1 == z._2) 1.0 else 0.0}
    bools.sum / bools.size
  }

  def confusion(xs: Vector[Vector[Double]], ys: Vector[Double]) = {
    assert(xs.size == ys.size, "Error: xs and ys need to have same length")
    val numClasses = param.numClasses
    val preds = xs.map(x => predict(x))
    val conf = Array.fill(numClasses)( Array.fill(numClasses)(0) )
    for ( (y,pred) <- ys zip preds) conf(y.toInt)(pred.toInt) += 1
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

  /** Leave one out Cross Validation. Probably not practical in streaming set-up. But useful for testing*/
  def leaveOneOutCV(xs: Vector[Vector[Double]], ys: Vector[Double], par: Boolean = false) = {
    assert(xs.size == ys.size, "Error: xs and ys need to have same length")
    val n = ys.size
    val inds = Vector.range(0,n)
    val conf = Array.fill(param.numClasses)( Array.fill(param.numClasses)(0) )
    for (i <- inds) {
      val orf = ClsForest(param,numTrees=100,par=par)
      val indsShuf = scala.util.Random.shuffle(0 to n-1) // important
      val trainInds = indsShuf.filter(_!=i)
      trainInds.foreach{ i => orf.update(xs(i),ys(i).toInt) }
      val pred = orf.predict(xs(i))
      val curr = conf(ys(i).toInt)(pred.toInt)
      conf(ys(i).toInt)(pred.toInt) = curr + 1
    }
    conf
  }
}

object RegForest { 
  def apply(param: Param, numTrees: Int = 100, par: Boolean = false) = new RegForest(param,numTrees,par) 
}
class RegForest (param: Param, numTrees: Int = 100, par: Boolean = false) extends ORForest (param,numTrees,par) { 
  // redefined methods:
  def newORTree = RegTree(param)
  def predict(x: Vector[Double]) = {
    val preds = forest.map(tree => tree.predict(x)) 
    preds.sum / preds.size
  }

  /** Returns prediction accuracy based on test input (xs) and test response (ys): Regression */
  def rmse(preds: Vector[Double], ys: Vector[Double]) = {
    assert(preds.size == ys.size, "Error: xs and ys need to have same length")
    val mse = preds.zip(ys).map{ z => (z._1-z._2) * (z._1-z._2) }.sum / preds.size
    scala.math.sqrt(mse)
  }

  def leaveOneOutPred(xs: Vector[Vector[Double]], ys: Vector[Double], par: Boolean = false) = {
    assert(xs.size == ys.size, "Error: xs and ys need to have same length")
    val n = ys.size
    val inds = Vector.range(0,n)
    val preds = {if (par) inds.par else inds} map { i =>
      val orf = RegForest(param,numTrees=100,par=par)
      val indsShuf = scala.util.Random.shuffle(0 to n-1) // important
      val trainInds = indsShuf.filter(_!=i)
      trainInds.foreach{ i => orf.update(xs(i),ys(i).toInt) }
      val pred = orf.predict(xs(i))
      pred
    }
    preds.toVector
  }

  def leaveOneOutRMSE(xs: Vector[Vector[Double]], ys: Vector[Double], par: Boolean = false) = {
    val preds = leaveOneOutPred(xs,ys,par)
    rmse(preds,ys)
  }
}
