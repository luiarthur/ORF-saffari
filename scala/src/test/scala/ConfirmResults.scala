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
  val param = Map[String,Double]("lam" -> 1, "numClass" -> y.toSet.size, 
                                 "alpha" -> n*.1, "beta" -> .04, 
                                 "numTests" -> 10, "gamma" -> .0)
  val inds = Vector.range(0,n)
  val orf = Forest(param,dataRange(X),numTrees=100,par=true)
  val predAccs = Vector.range(0,10) map { it =>
    val indShuff = scala.util.Random.shuffle(inds)
    indShuff.foreach{ i => orf.update(X(i),y(i).toInt) }
    println("mean tree size: " + orf.meanTreeSize)
    println("c: " + orf.forest(0).tree.elem.c.mkString(","))
    orf.predAccuracy(xtest,ytest) 
  }

  val conf = orf.confusion(xtest,ytest)
  print(Console.YELLOW)
  orf.printConfusion(conf)
  println(orf.predAccuracy(xtest,ytest) + Console.RESET)

}
