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

  //test("Forest Response") {
  //  val List(f1,f2,f3) = List(1, 1.0, Vector(0,1,2,3)) map { x => new Forest(Vector(1,2,3),x,10) }
  //  assert(f1.response == "Int" && f2.response == "Double" && f3.response == "InvalidResponseType")
  //}

  test("OT") {
    val param = Map[String,Double]("lam" -> 1, "numClass" -> 5, "alpha" -> 50, "beta" -> .1, "numTests" -> 10)
    //val ot = new OT(Info(10),OT(Info(9)),OT(Info(8)),param=param)
  }
}
