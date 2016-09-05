import numpy as np
from ort import ORT, dataRange, argmax
from math import sqrt

class ORF:
    def __init__(self,param,numTrees=100,par=False): # implement parallel FIXME
        self.param = param
        self.classify = param.has_key('numClasses') > 0
        self.numTrees = numTrees
        self.par = par
        self.forest = [ORT(param) for i in xrange(numTrees)]

    def update(self,x,y):
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
        ts = [ort.tree.size() for ort in self.forest]
        return sum(ts) / (self.numTrees*1.0)

    def meanNumLeaves(self):
        nl = [ort.tree.numLeaves() for ort in self.forest]
        return sum(nl) / (self.numTrees*1.0)

    def meanMaxDepth(self):
        md = [ort.tree.maxDepth() for ort in self.forest]
        return sum(md) / (self.numTrees*1.0)

    def __sd(xs): 
        n = len(xs) *1.0
        mu = sum(xs) / n
        return sqrt( sum(map(lambda x: (x-mu)*(x-mu),xs)) / n )

    def sdTreeSize(self):
        return([ort.tree.size() for ort in self.forest])

    def sdNumLEaves(self):
        return([ort.tree.numLeaves() for ort in self.forest])

    def sdMaxDepth(self):
        return([ort.tree.maxDepth() for ort in self.forest])
