from ort import ORT, dataRange, argmax
from math import sqrt

class ORF:
    def __init__(self,param,numTrees=100,ncores=0): # implement parallel at method update(self,x,y) FIXME
        self.param = param
        self.classify = param.has_key('numClasses') > 0
        self.numTrees = numTrees
        self.forest = [ORT(param) for i in xrange(numTrees)]
        self.ncores = ncores
        if ncores > 1:
            self.pool = Pool(ncores)

    def update(self,x,y): # implement parallel updates for each tree in forest here FIXME
        if self.ncores > 1:
            pass
        else:
            # sequential loop
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

    def sdTreeSize(self):
        return(sd([ort.tree.size() for ort in self.forest]))

    def sdNumLEaves(self):
        return(sd([ort.tree.numLeaves() for ort in self.forest]))

    def sdMaxDepth(self):
        return(sd([ort.tree.maxDepth() for ort in self.forest]))

# Other functions:
def sd(xs): 
    n = len(xs) *1.0
    mu = sum(xs) / n
    return sqrt( sum(map(lambda x: (x-mu)*(x-mu),xs)) / (n-1) )
