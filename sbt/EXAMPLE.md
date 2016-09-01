# Documentation / Usage example

Note that by executing `sbt doc`, the scaladoc will be generated in `target/scala-2.10/api`. You can view the generated docs by
starting a web server by typing in the terminal `python -m SimpleHTTPServer`. You can then view the page in a browser by typing
`localhost:8000`. Refer to the docs for more info on the methods of the RegForest and ClsForest objects.


## Regression Example

```scala
import ORF.models._ // this is how you import the ORF library. You usually won't need to import anything else
val Rand = new scala.util.Random(123) // create a random number generator with a random seed of 123

// create a weird non-linear function that takes a vector as input and returns a scalar
def f(x: Vector[Double]) = if (x(0) < x(1)) x(0)*math.sqrt(x(1)) else math.sqrt(x(1))*x(2)

// create training set
val ntrain = 1000
val xtrain = Vector.fill(ntrain)( Vector.fill(2)(Rand.nextDouble) ) // each training input must have type: Vector[Double]
val ytrain = xtrain map f // each training response must have type: Double

val ntest = 1000
val xtest = Vector.fill(ntest)( Vector.fill(2)(Rand.nextDouble) )
val ytest = xtest map f

/* Creating a parameter object
  minSamples: number of samples a node needs to see before splitting
  minGain: the minimum reduction of node impurity before node is split
  xrng: range of training inputs
*/
val param = Param(minSamples = 10, minGain = .01, xrng = dataRange(xtrain))

// creating online Regress Forest object. Using 10 Trees, and the parameters given above.
val orf = RegForest(param,numTrees=10,par=true) // setting parallel option to be true. trees in forest will be trained in parallel.
timer { // times how long the updates take
  for (i <- 0 until ntrain) orf.update(xtrain(i),ytrain(i)) // essentially: orf.update( newx: Vector[Double], newy: Double )
}

// post processing
val preds = orf.predicts(xtest)
val rmse = orf.rmse(preds,ytest)
```

***

## Classification Example

```scala
import ORF.models._
val Rand = new scala.util.Random(123)
def g(x: Vector[Double]) = if ( x(0)*x(0) + x(1)*x(1) < .5 ) 1.0 else 0.0

// create training set
val ntrain = 1000
val xtrain = Vector.fill(ntrain)( Vector.fill(2)(Rand.nextDouble) )
val ytrain = xtrain map g // for classification, responses must be integer-valued Doubles (0.0, 1.0, 2.0, ...) and must start from 0.0

val ntest = 1000
val xtest = Vector.fill(ntest)( Vector.fill(2)(Rand.nextDouble) )
val ytest = xtest map g

// same as before, but now we need to specify the number of classes is 2.
val param = Param(minSamples = 10, minGain = .01, xrng = dataRange(xtrain), numClasses = 2)
val orf = ClsForest(param,numTrees=10,par=true) // setting parallel option to be true. trees in forest will be trained in parallel.
timer { // times how long the updates take
  for (i <- 0 until ntrain) orf.update(xtrain(i),ytrain(i)) // essentially: orf.update( newx: Vector[Double], newy: Double )
}

// post processing
val preds = orf.predicts(xtest)
val conf = orf.confusion(xtest,ytest) // create a confusion matrix
orf.printConfusion(conf) // print confusion matrix
```
