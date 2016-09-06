import ORF.models._

def sd (xs: Vector[Double]) = {
  val n = xs.size
  val mu = xs.sum / n
  val mse = xs.map{x => (x-mu)*(x-mu)}.sum / (n-1)
  math.sqrt(mse)
}

// Classify
timer {
  val n = 1000
  def f(x: Vector[Double]) = if (x(0)*x(0) + x(1)*x(1) < 1) 1.0 else 0.0
  val X = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextGaussian))
  val y = X map f
  val param = Param(minSamples=10, minGain= .01, numClasses=2, xrng=dataRange(X))

  val ort = ClsTree(param)

  for (i <- 0 until n) ort.update(X(i),y(i))

  val pred = X map ort.predict
  val acc = {(pred zip y) map {z => if (z._1 == z._2) 1.0 else 0.0}}.sum / n

  println("max Depth: " + ort.tree.maxDepth)
  println("Accuracy: " + acc)
}
println

// Regression
timer {
  val n = 1000
  def f(x: Vector[Double]) = if (x(0)<x(1)) math.sin(x(0)) else math.cos(x(1)+math.Pi/2)
  val X = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextGaussian))
  val y = X map f
  val param = Param(minSamples=10, minGain= .01, xrng=dataRange(X))

  val ort = RegTree(param)

  for (i <- 0 until n) ort.update(X(i),y(i))

  val pred = X map ort.predict
  val mse = {(pred zip y) map {z => (z._1-z._2) * (z._1-z._2)}}.sum / n

  println("max Depth: " + ort.tree.maxDepth)
  println("RMSE: " + math.sqrt(mse))
}
println

// ORF Classify
timer {
  val n = 1000
  def f(x: Vector[Double]) = if (x(0)*x(0) + x(1)*x(1) < 1) 1.0 else 0.0
  val X = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextGaussian))
  val y = X map f
  val xtest = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextGaussian))
  val ytest = xtest map f

  val param = Param(minSamples=10, minGain= .01, numClasses=2, xrng=dataRange(X))

  val orf = ClsForest(param,numTrees=500,par=true)

  for (i <- 0 until n) orf.update(X(i),y(i))

  val conf = orf.confusion(xtest,ytest)
  println(ytest.sum)
  orf.printConfusion(conf)

  val pred = orf.predicts(xtest)
  val acc = {(pred zip ytest) map {z => if (z._1 == z._2) 1.0 else 0.0}}.sum / n

  println(Console.GREEN + "ORF Classify" + Console.RESET)
  println("Mean max Depth: " + orf.meanMaxDepth)
  println("Mean Size: " + orf.meanTreeSize)
  println("SD Size: " + orf.sdTreeSize)
  println("Accuracy: " + acc)
}
println

// ORF Regression
timer {
  val n = 1000
  def f(x: Vector[Double]) = if (x(0)<x(1)) math.sin(x(0)) else math.cos(x(1)+math.Pi/2)
  val X = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextGaussian))
  val y = X map f
  val xtest = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextGaussian))
  val ytest = xtest map f

  val param = Param(minSamples=10, minGain=0.0, xrng=dataRange(X), maxDepth=10)

  val orf = RegForest(param,numTrees=500,par=true)

  for (i <- 0 until n) orf.update(X(i),y(i))

  val preds = orf.predicts(xtest)
  val rmse = orf.rmse(preds,ytest)

  println(Console.GREEN + "ORF Regression" + Console.RESET)
  val sdx = orf.predStat(Vector(0.0,0.0), sd)
  println("f(0,0):          " + orf.predict(Vector(0.0,0.0))+ " +/- " + sdx)
  println("Mean Size:       " + orf.meanTreeSize)
  println("SD Size:         " + orf.sdTreeSize)
  println("Mean max Depth:  " + orf.meanMaxDepth)
  println("SD max Depth:    " + orf.sdMaxDepth)
  println("RMSE:            " + rmse)
}
