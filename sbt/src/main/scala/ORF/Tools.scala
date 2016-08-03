package ORF

object Tools {

  def dataRange(X: Vector[Vector[Double]]) = 
    Vector.range(0,X(0).size) map {j => 
      val colj = X map {x => x(j)}
      (colj.max, colj.min)
    }

  def dataClasses(y: Vector[Double]) = y.toSet.size

  def normalize(X: Vector[Vector[Double]]) = {
    val rng = dataRange(X)
    val n = X.size
    val k = X(0).size
    Vector.range(0,n) map { i =>
      Vector.range(0,k) map { j => 
        ( X(i)(j) - rng(j)._1 ) / ( rng(j)._2 - rng(j)._1)
      }
    }
  }

}
