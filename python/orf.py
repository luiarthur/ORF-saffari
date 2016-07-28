import numpy as np
import random

class ORF:
    def __init__(self, param, rng, numTrees=100):
        self.numTrees = numTrees
        self.param = param
        self.rng = rng
    #
    def update(x,y): 
        pass
    #
    def predict(self,x):
        pass

class Tree:
    def __init__(self, elem, left=None, right=None):
        self.elem = elem
        self.left = left
        self.right = right
    #
    def isLeaf(self):
        out = true if left == None and right == None else false
    #
    def draw(self):
        print "NEED TO IMPLEMENT THIS TO DRAW PRETTY TREES"
        pass

class Info:
    def __init__(self, numClass, numTest, xrng, splitDim=-1, splitLoc=0):
        self.xrng = xrng
        self.xdim = len(xrng)
        self.numClass = numClass
        self.numTest = numTest
        self.splitDim = splitDim
        self.splitLoc = splitLoc
        self.c = np.zeros(numClass)
        self.tests = [{'dim': random.randrange(self.xdim), 'loc': random.uniform(xrng[s][0],xrng[s][1]), \
                'cLeft': np.zeros(numClass), 'cRight': np.zeros(numClass)} for s in range(numTest)]
    #
    def numSamples(self):
        return self.c.sum()
    #
    def update(self,x,y):
        self.c[y] += 1
        for s in range(self.numTest):
            dim = self.tests[s]['dim']
            loc = self.tests[s]['loc']
            if x[dim] < loc:
                self.tests[s]['cLeft'][y] += 1
            else:
                self.tests[s]['cRight'][y] += 1

            #assert(test.cLeft.sum + test.cRight.sum <= self.c.sum)
    # 
    def pred(self):
        pass
    #
    def reset(self):
        self.c = 0


class ORT: # Online Random Tree
    def __init__(self,param,xrng): # eg: param = {'numClass': 10,'lam': 1,'minSamples': 5,'minGain': .1,'numTest': 10,'gamma': 0}
        self.param = param
        self.age = 0
        self.lam = param['lam']
        self.numTest = param['numTest']
        self.numClass = param['numClass']
        self.minSamples = param['minSamples']
        self.minGain = param['minGain']
        self.xrng = xrng
        self.tree = Tree(Info(self.numClass,self.numTest))
    #
    def __findLeaf(self,x):
        pass




### TEST:
xrng = np.array([[1,2],[3,4],[5,6]])
param = {'numClass': 10, 'lam': 1,'minSamples': 5,'minGain': .1,'numTest': 10,'gamma': 0}
ort = ORT(param,xrng)

numClass = 5
numTest = 3
I = Info(numClass,numTest,xrng); I.tests
I.update(xrng[0],0)
