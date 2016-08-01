object Confirmation {
  import ORF._

  val uspsTrain = scala.util.Random.shuffle(
    scala.io.Source.fromFile("src/test/resources/usps/train.csv").
    getLines.map(x=>x.split(" ").toVector.map(_.toDouble)).toVector)
  val uspsTest = scala.io.Source.fromFile("src/test/resources/usps/test.csv").
    getLines.map(x=>x.split(" ").toVector.map(_.toDouble)).toVector

  val n = uspsTrain.size
  val k = uspsTrain(0).size - 1
  val y = uspsTrain.map( _.head.toInt )
  val X = uspsTrain.map( _.tail )
  val xtest = uspsTest.map(x => x.tail)
  val ytest = uspsTest.map(yi => yi(0).toInt)

  println("Dim: " + X.size + "," + X(0).size)
  val inds = Vector.range(0,n)

  val ErrorRate = Timer.time {
    // par: 50s, not par: 88s
    (1 to 10).toList.par.map { it =>
      val currentInds = inds.take( (n*it*.1).toInt )
      val param = Param(lam = 1, numClasses = y.toSet.size, 
                        minSamples = n/10, minGain = .1, 
                        numTests = 10, gamma = 0)
      val orf = Forest(param,dataRange(X),numTrees=100,par=true)

      for (i <- 1 to 10) scala.util.Random.shuffle(currentInds).foreach( i => orf.update(X(i), y(i).toInt))
      val predAcc = orf.predAccuracy(xtest,ytest) 
      println("Iteration: "+Console.GREEN+it+Console.RESET)
      println("N_train: " + currentInds.size)
      println("mean number of leaves: " + orf.meanNumLeaves.toInt)
      println("Error Rate: " + ((1-predAcc)*10000).toInt / 100 + "%" )
      1-predAcc
    }.toList
  }
}
