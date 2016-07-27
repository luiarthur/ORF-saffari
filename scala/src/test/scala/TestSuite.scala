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
    val (trainInds, testInds) = inds.partition( _ % 2 == 0)
    trainInds.foreach{ i => orf.update(X(i),y(i).toInt) }
    //ot.tree.draw
    //(testInds).foreach( z => println("Pred: " + orf.predict(z._2) + ", Truth: " + z._1))
  }
}
