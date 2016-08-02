object Confirmation {
  import ORF._
  import scala.util.Random.shuffle

  // futures
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  val uspsTrain = scala.io.Source.fromFile("src/test/resources/usps/train.csv").
    getLines.map(x=>x.split(" ").toVector.map(_.toDouble)).toVector
  val uspsTest = scala.io.Source.fromFile("src/test/resources/usps/test.csv").
    getLines.map(x=>x.split(" ").toVector.map(_.toDouble)).toVector

  val y = uspsTrain.map( _.head.toInt )
  val X = uspsTrain.map( _.tail )
  val xtest = uspsTest.map( _.tail )
  val ytest = uspsTest.map( _.head.toInt )

  println("Dim: " + X.size + "," + X(0).size)

  type VDD = Vector[Vector[Double]]
  type VI = Vector[Int]

  def train(X: VDD, y: VI, xtest: VDD, ytest: VI, percentage: Double, shuffles: Int = 10, lam: Double = 1, gamma: Double = 0) = {
    val N = y.size
    val k = y.toSet.size
    val inds = Vector.range(0,N)
    val xrng = dataRange(X)

    val currentInds = shuffle(inds).take( (N*percentage).toInt )
    val n = currentInds.size
    val param = Param(lam = lam, numClasses = k, 
                      minSamples = n/10, minGain = .1, 
                      numTests = 10, gamma = gamma)

    val orf = Forest(param,xrng,numTrees=100,par=true)
    for (i <- 1 to shuffles) shuffle(currentInds).foreach( i => orf.update(X(i), y(i).toInt))
    val predAcc = orf.predAccuracy(xtest,ytest) 

    println("Percentage of Data: "+Console.GREEN+percentage+Console.RESET)
    println("N_train: " + currentInds.size)
    println("Shuffles: " + shuffles)
    println("mean number of leaves: " + orf.meanNumLeaves.toInt)
    println("mean max depth: " + orf.meanMaxDepth.toInt)
    println("Error Rate: " + Console.BLUE + ((1-predAcc)*10000).toInt / 100 + "%" + Console.RESET)
    1-predAcc
  }

  val errorRate = Timer.time {
    Vector.tabulate(10)(i => (i+1) / 10.0).par map { p =>
      train(X,y,xtest,ytest,percentage=p,shuffles=10)
    }
  }.toList


  val f = future {
    Vector.range(1,11).par map { 
      shuff => train(X,y,xtest,ytest,percentage=1,shuffles=shuff) }
  }
  f.value.get

  train(X,y,xtest,ytest,percentage=1,shuffles=10,gamma=1/90000.0)
}
