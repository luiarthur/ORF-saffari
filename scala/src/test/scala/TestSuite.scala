import org.scalatest.FunSuite

class TestSuite extends FunSuite {
  import ORF._

  test("Drawing / testing trees") {
    val x = Tree(1,Tree(202),Tree(303))
    x.draw
    x.left.draw
    println
    assert(!x.isLeaf && x.left.isLeaf)
  }

  test("ORT") {
    val iris = scala.io.Source.fromFile("src/test/resources/iris.csv").getLines.map(x=>x.split(",").toVector.map(_.toDouble)).toVector
    val n = iris.size
    val k = iris(0).size - 1
    val y = iris.map( yi => {yi(k) - 1}.toInt )
    val X = iris.map(x => x.take(k))
    val param = Map[String,Double]("lam" -> 1, "numClass" -> y.toSet.size, "alpha" -> 5, "beta" -> .1, "numTests" -> 3)

    val orf = Forest(param,dataRange(X))
    //assert(orf.forest(0) != orf.forest(1))

    val inds = scala.util.Random.shuffle(0 to n-1) // important that order is shuffled

    val (testInds, trainInds) = inds.partition( _ % 5 == 0)
    //val ip = inds.zipWithIndex.partition( _._2 % 5 == 0)
    //val (testInds, trainInds) = (ip._1.unzip._1,ip._2.unzip._1)
    //print(trainInds.size,testInds.size)

    trainInds.foreach{ i => orf.update(X(i),y(i).toInt) }
    orf.forest(0).tree.draw
    orf.forest(1).tree.draw
    val xtest = testInds.map(X(_)).toVector
    val ytest = testInds.map(y(_)).toVector
    val conf = orf.confusion(xtest,ytest)
    orf.printConfusion(conf)
  }
}
