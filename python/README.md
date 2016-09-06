# Documentation / Usage Example

## Directory Structure

The `src` directory contains 3 files: (1) `tree.py`, (2) `ort.py`, and (3) `orf.py`. I will first exlain breifly what they contain.

`tree.py` contains a simple implementation of a binary tree. It is meant to be for general use. Note that the tree expects exactly 2 or 0 children. Care has been taken in the implementation to ensure that the user cannot *create* a tree with only one child (either only a left child, or only a right child). The correct way to initialize a tree is either `Tree(1)` which creates a tree with an integer element 1; or `Tree(1,Tree(2),Tree(3)` which creates a tree with an integer element 1, left child a tree with integer element 2, and and right child with integer element 3. For more details, read the docstring in python by importing tree.py:

```python
import tree
help(tree)
```

`ort.py` and `orf.py` contain respectively the implementations for the on-line random tree and forest by Amir Saffari. Once again, see the docstring for more info. Note that the trees in the forest are independent and so they *can be* updated in parallel. However, I have not found a simple way of doing so. I have added a `FIXME` marker to indicate that the update method of the `ORF` class in `orf.py` can be reimplemented to be parallelized.

## Example

To import the library into python, you will need to add the path to the `src` directory to the system search path in python. Do this by

```python
import sys
sys.path.append("../src")
```

What follows is what I think is a self-explanatory usage example. Note that the OnlineRF library does not depend on external
packages (i.e. it doesn't use np) but the example uses numpy for creating matrices for simplicity.

### Classification Example:
```python
import numpy as np
import math
from OnlineRF import ORF, dataRange
#from OnlineRF import ORT # if you want online random tree

def f(x):
    return int(x[0]*x[0] + x[1]*x[1] < 1)

n = 1000
X = np.random.randn(n,2)
y = map(f,X)
param = {'minSamples': 10, 'minGain': .01, 'numClasses': 2, 'xrng': dataRange(X)}
orf = ORF(param,numTrees=50)
for i in range(n):
    orf.update(X[i,:],y[i])

xtest = np.random.randn(n,2)
ytest = map(f,xtest)
preds = orf.predicts(xtest)

predAcc = sum(map(lambda z: int(z[0] == z[1]), zip(preds,ytest))) / float(len(preds))
conf = orf.confusion(xtest,ytest)
orf.printConfusion(conf)
#y\pred   0      1
#0       603     10
#1       26      361
print "Accuracy: " + str(round(predAcc * 100,2)) + "%"
# Accuracy: 97.0%
```

### Regression Example:
```python
import numpy as np
import math
from OnlineRF import ORF, dataRange

def g(x):
    return math.sin(x[0]) if x[0]<x[1] else math.cos(x[1]+math.pi/2)

n = 1000
X = np.random.randn(n,2)
y = map(g,X)
param = {'minSamples': 10, 'minGain': 0, 'xrng': dataRange(X), 'maxDepth': 10}
xtest = np.random.randn(n,2)
ytest = map(g,xtest)
orf = ORF(param,numTrees=50)

for i in range(n):
    orf.update(X[i,:],y[i])

preds = orf.predicts(xtest)
sse = sum( map(lambda z: (z[0]-z[1])*(z[0]-z[1]) , zip(preds,ytest)) )
rmse = math.sqrt(sse / float(len(preds)))
print "RMSE: " + str(round(rmse,2))
# RMSE: 0.22
```
