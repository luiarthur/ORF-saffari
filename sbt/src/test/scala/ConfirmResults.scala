object Confirmation {
  import ORF.models._
  import scala.util.Random.shuffle

  val uspsTrain = scala.io.Source.fromFile("src/test/resources/usps/train.csv").
    getLines.map(x=>x.split(" ").toVector.map(_.toDouble)).toVector
  val uspsTest = scala.io.Source.fromFile("src/test/resources/usps/test.csv").
    getLines.map(x=>x.split(" ").toVector.map(_.toDouble)).toVector

  val ytrain = uspsTrain.map( _.head )
  val xtrain = uspsTrain.map( _.tail )
  val xtest = uspsTest.map( _.tail )
  val ytest = uspsTest.map( _.head )

  println("Dim: " + xtrain.size + "," + xtrain(0).size)

  type VDD = Vector[Vector[Double]]
  type VD = Vector[Double]

  def train(xtrain: VDD, ytrain: VD, xtest: VDD, ytest: VD, percentage: Double, shuffles: Int = 10) = {
    val N = xtrain.size
    val xrng = dataRange(xtrain ++ xtest)

    val currentObs = shuffle(xtrain zip ytrain).take( (N*percentage).toInt )
    val n = currentObs.size
    val param = Param(minSamples=n/100, minGain=.1, xrng=xrng, numClasses=ytrain.toSet.size)

    val orf = ClsForest(param,numTrees=100,par=true)
    for (j <- 1 to shuffles) shuffle(currentObs).foreach( o => orf.update(o._1, o._2))
    val predAcc = orf.predAcc(xtest,ytest) 

    println("Percentage of Data: "+Console.GREEN+percentage+Console.RESET)
    println("N_train: " + currentObs.size)
    println("Shuffles: " + shuffles)
    println("mean number of leaves: " + orf.meanNumLeaves.toInt)
    println("mean max depth: " + orf.meanMaxDepth.toInt)
    println("Error Rate: " + Console.BLUE + ((1-predAcc)*10000).toInt / 100 + "%" + Console.RESET)
    1-predAcc
  }

  val errorRate = {
    Vector.tabulate(10)(i => (i+1) / 10.0).par map { p =>
      train(xtrain,ytrain,xtest,ytest,percentage=p,shuffles=10)
    }
  }.toList

  errorRate foreach println
}
