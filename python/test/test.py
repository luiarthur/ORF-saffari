#!/usr/bin/env python

import unittest, sys
import numpy as np
sys.path.append("../src")
import math

def mean(x):
    return sum(x) / (len(x)*1.0)

from ort import ORT, dataRange
from orf import ORF, sd
from bcolors import bcolors as bc

print bc.HEADER + "Starting Test..." + bc.ENDC

def warn(msg="wtf?"):
    return bc.FAIL + msg + bc.ENDC

class Tests(unittest.TestCase):

    from tree import Tree
    global t1,t2,t3,t4

    t1 = Tree(1)
    t1.draw()
    t2 = Tree(1,Tree(2),Tree(3))
    t2.draw()
    t3 = Tree(1,t2,Tree(4))
    t3.draw()
    t4 = Tree(1,t2,t3)
    t4.draw()

    def test1(self,msg=warn("Error in subtree equality")):
        self.assertTrue(t2==t3.left and t2==t4.left and t4.right==t3, msg)

    def test2(self,msg=warn("Error in tree.size")):
        self.assertTrue(t2.size()==3 and t4.size()==9, msg)

    def test3(self,msg=warn("Error in tree.numLeaves")):
        self.assertTrue(t2.numLeaves()==2 and t4.numLeaves()==5 and t1.numLeaves() == 1, msg)

    def test4(self,msg=warn("Error in tree.maxDepth")):
        self.assertTrue(t1.maxDepth() == 1 and t2.maxDepth()==2 and t4.maxDepth()==4,msg)

    def test5(self,msg=warn("test ORT Classify")):
        def f(x):
            return 1 if x[0]*x[0] + x[1]*x[1] < .5*.5 else 0
        n = 1000
        X = np.random.randn(n,2)
        y = map(f,X)
        param = {'minSamples': 10, 'minGain': .01, 'numClasses': 2, 'xrng': dataRange(X), 'maxDepth': 5}
        ort = ORT(param)
        map(lambda i: ort.update(X[i,:],y[i]), range(n))
        #ort.draw()
        preds = map(lambda i: ort.predict(X[i,:]), range(n))
        acc = map(lambda z: z[0]==z[1] , zip(preds,y))
        print "ORT Classify:"
        print "Accuracy: " + str(mean(acc))
        print "max depth: " + str(ort.tree.maxDepth())
        print

    def test6(self,msg=warn("test ORT Regression")):
        def f(x):
            return math.sin(x[0]) if x[0]<x[1] else math.cos(x[1]+math.pi/2)
        n = 1000
        X = np.random.randn(n,2)
        y = map(f,X)
        param = {'minSamples': 10, 'minGain': .01, 'xrng': dataRange(X), 'maxDepth': 5}
        ort = ORT(param)
        for i in range(n):
            ort.update(X[i,:],y[i])
        #ort.draw()
        preds = map(lambda i: ort.predict(X[i,:]), range(n))
        mse = mean(map(lambda z: (z[0]-z[1])*(z[0]-z[1]) , zip(preds,y)))
        print "ORT Regression:"
        print "RMSE: " + str(math.sqrt(mse))
        print "max depth: " + str(ort.tree.maxDepth())
        print
        #print "Root counts: " + str(vars(ort.tree.elem.stats))

    def test7(self,msg=warn("test ORF Classify")):
        def f(x):
            return 1 if x[0]*x[0] + x[1]*x[1] < .5*.5 else 0
        n = 1000
        X = np.random.randn(n,2)
        y = map(f,X)
        param = {'minSamples': 10, 'minGain': .01, 'numClasses': 2, 'xrng': dataRange(X)}
        orf = ORF(param,numTrees=500)
        for i in range(n):
            orf.update(X[i,:],y[i])

        xtest = np.random.randn(n,2)
        ytest = map(f,xtest)
        preds = orf.predicts(xtest)

        acc = map(lambda z: z[0]==z[1] , zip(preds,ytest))
        print "ORF Classify:"
        print "Mean max depth: " + str(orf.meanMaxDepth())
        print "Mean Size: " + str(orf.meanTreeSize())
        print "SD Size: " + str(orf.sdTreeSize())
        print "Accuracy: " + str(mean(acc))
        print

    def test8(self,msg=warn("test ORF Regression")):
        def f(x):
            return math.sin(x[0]) if x[0]<x[1] else math.cos(x[1]+math.pi/2)
        n = 1000
        X = np.random.randn(n,2)
        y = map(f,X)
        param = {'minSamples': 10, 'minGain': 0, 'xrng': dataRange(X), 'maxDepth': 10}
        xtest = np.random.randn(n,2)
        ytest = map(f,xtest)
        orf = ORF(param,numTrees=500)

        for i in range(n):
            orf.update(X[i,:],y[i])

        preds = orf.predicts(xtest)

        mse = mean( map(lambda z: (z[0]-z[1])*(z[0]-z[1]) , zip(preds,ytest)) )
        print "ORF Regression:"
        print "f(0,0):          " + str(orf.predict([0,0])) + " +/- " + str(orf.predStat([0,0],sd))
        print "Mean size:       " + str(orf.meanTreeSize())
        print "SD size:         " + str(orf.sdTreeSize())
        print "Mean max depth:  " + str(orf.meanMaxDepth())
        print "SD max depth:    " + str(orf.sdMaxDepth())
        print "RMSE:            " + str(math.sqrt(mse))
        print

if __name__=='__main__':
    unittest.main()
