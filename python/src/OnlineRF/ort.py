from tree import Tree
from math import e, sqrt, exp
from utils import argmax, log2
import random

class ORT:
    """
    Examples:
        xrng = [[x0_min,x0_max], [x1_min,x1_max], [x2_min,x2_max]]
        param = {'minSamples': 5, 'minGain': .1, 'numClasses': 10, 'xrng': xrng}
    """
    def __init__(self,param):
        self.param = param
        self.__age = 0
        self.minSamples = param['minSamples']
        self.minGain = param['minGain']
        self.xrng = param['xrng']
        self.gamma = param['gamma'] if param.has_key('gamma') else 0
        self.numTests = param['numTests'] if param.has_key('numTests') else 10
        self.numClasses = param['numClasses'] if param.has_key('numClasses') else 0
        self.maxDepth = param['maxDepth'] if param.has_key("maxDepth") else 30 # This needs to be implemented to restrain the maxDepth of the tree. FIXME
        self.tree = Tree( Elem(param=param) )

    def draw(self):
        print self.tree.treeString(fun=True)

    def update(self,x,y):
        k = self.__poisson(1)
        if k == 0:
            self.__updateOOBE(x,y)
        else:
            for u in xrange(k):
                self.__age += 1
                (j,depth) = self.__findLeaf(x,self.tree)
                j.elem.update(x,y)
                #if j.elem.numSamplesSeen > self.minSamples and depth < self.maxDepth: # FIXME: which is the correct approach?
                if j.elem.stats.n > self.minSamples and depth < self.maxDepth:
                    g = self.__gains(j.elem)
                    if any([ gg >= self.minGain for gg in g ]):
                        bestTest = j.elem.tests[argmax(g)]
                        j.elem.updateSplit(bestTest.dim,bestTest.loc)
                        j.updateChildren( Tree(Elem(self.param)), Tree(Elem(self.param)) )
                        j.left.elem.stats = bestTest.statsL
                        j.right.elem.stats = bestTest.statsR
                        j.elem.reset()

    def predict(self,x):
        return self.__findLeaf(x,self.tree)[0].elem.pred() # [0] returns the node, [1] returns the depth

    def __gains(self,elem):
        tests = elem.tests
        def gain(test):
            statsL, statsR = test.statsL,test.statsR
            nL,nR = statsL.n,statsR.n
            n = nL + nR + 1E-9
            lossL = 0 if nL==0 else statsL.impurity()
            lossR = 0 if nR==0 else statsR.impurity()
            g = elem.stats.impurity() - (nL/n) * lossL - (nR/n)  * lossR
            return 0 if g < 0 else g
        return map(gain, tests)

    def __findLeaf(self, x, tree, depth=0):
        if tree.isLeaf(): 
            return (tree,depth)
        else:
            (dim,loc) = tree.elem.split()
            if x[dim] < loc:
                return self.__findLeaf(x,tree.left,depth+1)
            else:
                return self.__findLeaf(x,tree.right,depth+1)

    def __poisson(self,lam=1): # fix lamda = 1
      l = exp(-lam)
      def loop(k,p):
          return loop(k+1, p * random.random()) if (p > l) else k - 1
      return loop(0,1)

    def __updateOOBE(self,x,y):
        """
        Needs to be implemented
        """
        pass

class SuffStats:
    def __init__(self,numClasses=0,sm=0.0,ss=0.0):
        self.n = 0
        self.__classify = numClasses > 0
        self.eps = 1E-10
        if numClasses > 0:
            self.counts = [0] * numClasses
        else:
            self.sum = sm
            self.ss = ss

    def update(self,y):
        self.n += 1
        if self.__classify:
            self.counts[y] += 1
        else:
            self.sum += y
            self.ss += y*y

    def reset(self):
        self.n = None
        self.eps = None
        self.__classify = None
        if self.__classify:
            self.counts = None
        else:
            self.sum = None
            self.ss = None

    def pred(self):
        if self.__classify:
            return argmax(self.counts)
        else:
            return self.sum / (self.n+self.eps)
    
    def impurity(self):
        n = self.n + self.eps
        if self.__classify:
            return sum(map(lambda x: -x/n * log2(x/n + self.eps), self.counts)) # entropy
        else:
            prd = self.pred()
            return sqrt( self.ss/n - prd*prd ) # sd of node

class Test:
    def __init__(self,dim,loc,numClasses):
        self.__classify = numClasses > 0
        self.statsL = SuffStats(numClasses=numClasses)
        self.statsR = SuffStats(numClasses=numClasses)
        self.dim = dim
        self.loc = loc

    def update(self,x,y):
        if x[self.dim] < self.loc:
            self.statsL.update(y) 
        else:
            self.statsR.update(y)

class Elem: #HERE
    def __init__(self,param,splitDim=-1,splitLoc=0,numSamplesSeen=0):
        self.xrng = param['xrng']
        self.xdim = len(param['xrng'])
        self.numClasses = param['numClasses'] if param.has_key('numClasses') else 0
        self.numTests = param['numTests'] if param.has_key('numTests') else 10
        self.splitDim = splitDim
        self.splitLoc = splitLoc
        self.numSamplesSeen = numSamplesSeen
        self.stats = SuffStats(self.numClasses)
        self.tests = [ self.generateTest() for i in range(self.numTests) ]

    def reset(self):
        self.stats = None #self.stats.reset()
        self.tests = None

    def generateTest(self):
        dim = random.randrange(self.xdim)
        loc = random.uniform(self.xrng[dim][0],self.xrng[dim][1])
        return Test(dim, loc, self.numClasses)

    def toString(self):
        return str(self.pred()) if self.splitDim == -1 else "X" + str(self.splitDim+1) + " < " + str(round(self.splitLoc,2))

    def pred(self):
        return self.stats.pred()
    
    def update(self,x,y):
        self.stats.update(y)
        self.numSamplesSeen += 1
        for test in self.tests:
            test.update(x,y)

    def updateSplit(self,dim,loc):
        self.splitDim, self.splitLoc = dim, loc

    def split(self):
        return (self.splitDim,self.splitLoc)
