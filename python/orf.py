import numpy as np
import random

def getRange(X, pad=.1):
    k = len(X[0])
    rng = np.zeros([k,2])
    for j in range(k):
        rng[j,0] = X[:,j].min() - pad
        rng[j,1] = X[:,j].max() + pad
    #
    return rng

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
        return self.left == None and self.right == None
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
            assert(self.tests[s]['cLeft'].sum() + self.tests[s]['cRight'].sum() <= self.c.sum())
    # 
    def pred(self):
        return self.c.argmax()
    #
    def reset(self):
        self.c = 0
        self.tests = 0


class ORT: # Online Random Tree
    """
    Examples:
        param = {'numClass': 10,'lam': 1,'minSamples': 5,'minGain': .1,'numTest': 10,'gamma': 0}
        xrng = np.array([[x0_min,x0_max], [x1_min,x1_max], [x2_min,x2_max]])
    """
    def __init__(self,param,xrng): 
        self.param = param
        self.age = 0
        self.lam = param['lam']
        self.numTest = param['numTest']
        self.numClass = param['numClass']
        self.minSamples = param['minSamples']
        self.minGain = param['minGain']
        self.gamma = param['gamma']
        self.xrng = xrng
        self.tree = Tree(Info(self.numClass, self.numTest, xrng))
    #
    def __findLeaf(self, x, tree):
        if tree.isLeaf(): 
            return tree
        else:
            dim = tree.elem.splitDim
            loc = tree.elem.splitLoc
            if x[dim] < loc:
                self.__findLeaf(x,tree.left)
            else:
                self.__findLeaf(x,tree.right)
    #
    def predict(self, x):
        return self.__findLeaf(x,self.tree).elem.pred()
    #
    def update(self, x, y):
        k = np.random.poisson(1)
        if k > 0:
            for u in range(k):
                print "HERE0"
                self.age += 1
                j = self.__findLeaf(x,self.tree)
                j.elem.update(x,y)
                print "HERE00"
                if j.elem.numSamples() > self.minSamples:
                    g = self.__gains(j.elem)
                    if any(g > self.minGain):
                        i = g.argmax()
                        bestTest = j.elem.tests[i]
                        print "HERE"
                        j.left = Tree( Info(self.numClass, self.numTest, self.xrng) )
                        j.right = Tree( Info(self.numClass, self.numTest, self.xrng) )
                        print "HERE1"
                        j.elem.splitDim = bestTest['dim']
                        j.elem.splitLoc = bestTest['loc']
                        print "HERE2"
                        j.left.elem.c = bestTest['cLeft']
                        j.right.elem.c = bestTest['cRight']
                        print "HERE3"
                        #j.elem.reset()
                        print "HERE4"
        else: # k = 0. Estimate OOBE: Used for temporal knowledge weighting. (Algorithm 2 of Saffari paper).
            pass
    #
    def __loss(self,c):
        n = c.sum() + .0
        assert(n > 0, "Error in ORT.__loss(): n <= 0")
        p = c / n
        ps = p * (1-p) # gini
        # - p * np.log(p) # Entropy
        return ps.sum()
    #
    def __gains(self,info):
        tests = info.tests
        n = info.numSamples() + .0
        out = np.zeros( self.numTest )
        for i in range( self.numTest ):
            cL = tests[i]['cLeft']
            cR = tests[i]['cRight']
            out[i] = self.__loss(info.c) - cL.sum() / n * self.__loss(cL) - cR.sum() / n * self.__loss(cR)
        return out

### TEST:
x = np.array([1.5,2.5,3.5])
xrng = np.array([[1,2],[3,4],[5,6]])
assert( len(x) == len(xrng) )


iris = np.genfromtxt('../scala/src/test/resources/iris.csv', delimiter=',')
iris[:,4] -= 1
param = {'numClass': np.unique(iris[:,4]).size, 'lam': 1, 'minSamples': 5, 'minGain': .1, 'numTest': 3, 'gamma': 0}
np.random.shuffle(iris)
X = iris[:,0:3]
y = iris[:,4]
ort = ORT(param, getRange(X))
for row in iris:
    xx = row[0:3]
    yy = row[4]
    ort.update(x,y)

ort.predict(X[0,:])
