import org.scalatest.FunSuite

class TestSuite extends FunSuite {
  def round(x: Double, d: Int = 2) = (scala.math.pow(10,d) * x).toInt / scala.math.pow(10,d)

  test("Drawing / testing trees") {
    import ORF.Tree
    val x = Tree(1,Tree(202),Tree(303))
    x.draw
    x.left.draw
    println
    assert(!x.isLeaf && x.left.isLeaf)
  }

  test("max depth") {
    import ORF.Tree
    val t = Tree(1)
    val t2 = Tree(1,Tree(2),Tree(3))
    val t3 = Tree(1,Tree(4,t2,t),Tree(5,Tree(6),t2))
    val t4 = t3.copy()
    //t4.updateChildren(Tree(5),Tree(6))
    //t4.right = Tree(5)
    //assert(t.maxDepth == 1 && t2.maxDepth == 2 && t3.maxDepth == 4 && t4.maxDepth == 4)
    assert(t.maxDepth == 1 && t2.maxDepth == 2 && t3.maxDepth == 4)
  }

  //import ORF.Tools._
  val iris = scala.util.Random.shuffle(
    scala.io.Source.fromFile("src/test/resources/iris.csv").getLines.map(x=>x.split(",").toVector.map(_.toDouble)).toVector)
  val n = iris.size
  val y = iris.map( yi => yi.last - 1)
  val X = iris.map(x => x.dropRight(1))
  val inds = (0 to n-1)
  val (testInds, trainInds) = inds.partition( _ % 5 == 0)
  val xtest = testInds.map(X(_)).toVector
  val ytest = testInds.map(y(_)).toVector

  test("Template") {
    import ORF.models._
    val param = Param(minSamples = 5, minGain = .1, xrng = dataRange(X), numClasses=y.toSet.size)
    val ort = ClsTree(param)
    inds foreach { i => ort.update(X(i),y(i)) }
    ort.tree.draw
  }
  
  test("Forest") {
    import ORF.models._
    val param = Param(minSamples = 5, minGain = .1, xrng = dataRange(X), numClasses=y.toSet.size)
    val orf = ClsForest(param,par=true)
    //trainInds foreach { i => orf.update(X(i),y(i)) }
    //println("Test Accuracy: " + orf.predAcc(xtest,ytest))
    val conf = orf.leaveOneOutCV(X,y,par=true)
    orf.printConfusion(conf)
  }


  test("ORF Regression") {
    import ORF.models._
    val randErr = inds map { i => (scala.util.Random.nextDouble - .5)/10 }
    val param = Param(minSamples = 5, minGain = .1, xrng = dataRange(X))
    val orf = RegForest(param,par=true)
    //trainInds foreach { i => orf.update(X(i), y(i) + randErr(i))}
    //println(orf.predicts(xtest))
    val preds = orf.leaveOneOutPred(X,y,par=true)
    (preds zip y) foreach { z =>  println(round(z._1) +", " + z._2) }
    val looRMSE = orf.leaveOneOutRMSE(X,y,par=true)
    println("Leave One Out RMSE: " + looRMSE)
  }

  test("ORF Classification USPS") {
    import ORF.models._

    val uspsTrain = scala.io.Source.fromFile("src/test/resources/usps/train.csv").
      getLines.map(x=>x.split(" ").toVector.map(_.toDouble)).toVector
    val uspsTest = scala.io.Source.fromFile("src/test/resources/usps/test.csv").
      getLines.map(x=>x.split(" ").toVector.map(_.toDouble)).toVector
    val xtest = uspsTest map { _.tail }
    val ytest = uspsTest map { _.head }

    val xrng = dataRange( (uspsTrain map { _.tail }) ++ (uspsTest map { _.tail }) )
    val param = Param(minSamples = uspsTrain.size/10, minGain = .1, xrng = xrng, numClasses=10) // 87% Accuracy
    val orf = ClsForest(param,par=true)

    for (i <- 1 to 10) {
      scala.util.Random.shuffle(uspsTrain) foreach { o => orf.update(o.tail, o.head) }
      println("i: " + i + Console.GREEN + " | Error: " + { 1-orf.predAcc(xtest,ytest) } + Console.RESET)
    }

    val conf = orf.confusion( xtest, ytest )

    println
    orf.printConfusion(conf)
    println("USPS Prediction Accuracy: " + orf.predAcc(xtest,ytest))
    println("mean treesize: " + orf.meanTreeSize)
    println("mean max depth: " + orf.meanMaxDepth)
  }
}
