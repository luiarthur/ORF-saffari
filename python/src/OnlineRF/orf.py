from ort import ORT
from math import sqrt
from utils import argmax

class ORF:
    def __init__(self,param,numTrees=100,ncores=0): # implement parallel at method update(self,x,y) FIXME
        self.param = param
        self.classify = param.has_key('numClasses') > 0
        self.numTrees = numTrees
        self.forest = [ORT(param) for i in xrange(numTrees)]
        self.ncores = ncores

    def update(self,x,y): # implement parallel updates for each tree in forest here FIXME
        if self.ncores > 1:
            # parallel updates
            pass
        else:
            # sequential updates
            for tree in self.forest:
                tree.update(x,y)

    def predict(self,x):
        preds = [tree.predict(x) for tree in self.forest]
        if self.classify:
            cls_counts = [0] * self.param['numClasses']
            for p in preds:
                cls_counts[p] += 1
            return argmax(cls_counts)
        else:
            return sum(preds) / (len(preds)*1.0)

    def predicts(self,X):
        return [self.predict(x) for x in X]

    def predStat(self,x,f):
        return f([tree.predict(x) for tree in self.forest])

    def meanTreeSize(self):
        return mean(map(lambda ort: ort.tree.size(), self.forest))

    def meanNumLeaves(self):
        return mean(map(lambda ort: ort.tree.numLeaves(), self.forest))

    def meanMaxDepth(self):
        return mean(map(lambda ort: ort.tree.maxDepth(), self.forest))

    def sdTreeSize(self):
        return sd([ort.tree.size() for ort in self.forest])

    def sdNumLEaves(self):
        return sd([ort.tree.numLeaves() for ort in self.forest])

    def sdMaxDepth(self):
        return sd([ort.tree.maxDepth() for ort in self.forest])
    
    def confusion(self,xs,ys):
        n = self.param['numClasses']
        assert n > 1, "Confusion matrices can only be obtained for classification data." 
        preds = self.predicts(xs)
        conf = [[0] * n for i in range(n)]
        for (y,p) in zip(ys,preds):
            conf[y][p] += 1
        return conf
    
    def printConfusion(self,conf):
        print    "y\pred\t " + "\t".join(map(str,range(self.param['numClasses'])))
        i = 0
        for row in conf:
            print str(i) + "\t" + "\t".join(map(str,row))
            i += 1

# Other functions:
def mean(xs):
    return sum(xs) / (len(xs)*1.0)

def sd(xs): 
    n = len(xs) *1.0
    mu = sum(xs) / n
    return sqrt( sum(map(lambda x: (x-mu)*(x-mu),xs)) / (n-1) )
