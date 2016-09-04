import ORF.models._

// Classify
{
  val n = 1000
  def f(x: Vector[Double]) = if (x(0)*x(0) + x(1)*x(1) < .5*.5) 1.0 else 0.0
  val X = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextDouble))
  val y = X map f
  val param = Param(minSamples=10, minGain= .01, numClasses=2, xrng=dataRange(X))

  val ort = ClsTree(param)

  for (i <- 0 until n) ort.update(X(i),y(i))

  val pred = X map ort.predict
  val acc = {(pred zip y) map {z => if (z._1 == z._2) 1.0 else 0.0}}.sum / n

  println("max Depth: " + ort.tree.maxDepth)
  println("Accuracy: " + acc)
  println
}

// Regression
{
  val n = 1000
  def f(x: Vector[Double]) = if (x(0)<x(1)) math.sin(x(0)) else math.cos(x(1)+math.Pi/2)
  val X = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextDouble))
  val y = X map f
  val param = Param(minSamples=10, minGain= .01, xrng=dataRange(X))

  val ort = RegTree(param)

  for (i <- 0 until n) ort.update(X(i),y(i))

  val pred = X map ort.predict
  val mse = {(pred zip y) map {z => (z._1-z._2) * (z._1-z._2)}}.sum / n

  println("max Depth: " + ort.tree.maxDepth)
  println("RMSE: " + math.sqrt(mse))
  println
}

// ORF Classify
{
  val n = 1000
  def f(x: Vector[Double]) = if (x(0)*x(0) + x(1)*x(1) < .5*.5) 1.0 else 0.0
  val X = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextDouble))
  val y = X map f
  val param = Param(minSamples=10, minGain= .01, numClasses=2, xrng=dataRange(X))

  val orf = ClsForest(param)

  for (i <- 0 until n) orf.update(X(i),y(i))

  val pred = orf.predicts(X)
  val acc = {(pred zip y) map {z => if (z._1 == z._2) 1.0 else 0.0}}.sum / n

  println("Mean max Depth: " + orf.meanMaxDepth)
  println("Accuracy: " + acc)
  println
}

// ORF Regression
if (false) {
  val n = 1000
  def f(x: Vector[Double]) = if (x(0)<x(1)) math.sin(x(0)) else math.cos(x(1)+math.Pi/2)
  val X = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextDouble))
  val y = X map f
  val xtest = Vector.fill(n)(Vector.fill(2)(scala.util.Random.nextDouble))
  val ytest = xtest map f

  val param = Param(minSamples=10, minGain= .01, xrng=dataRange(X))

  val orf = RegForest(param)

  for (i <- 0 until n) orf.update(X(i),y(i))

  val preds = orf.predicts(X)
  val rmse = orf.rmse(preds,ytest)

  println("max Depth: " + orf.meanMaxDepth)
  println("RMSE: " + rmse)
 
}
