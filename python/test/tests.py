#!/usr/bin/env python

import unittest, sys
import numpy as np
sys.path.append("../src")

from ort import ORT, dataRange
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

    def test5(self,msg=warn("test ORT")):
        def f(x):
            return 1 if x[0]*x[0] + x[1]*x[1] < .5*.5 else 0
        n = 1000
        X = np.random.randn(n,2)
        y = [ f(X[i,:]) for i in xrange(n) ]
        param = {'minSamples': 10, 'minGain': .01, 'numClasses': 2, 'xrng': dataRange(X), 'maxDepth': 5}
        ort = ORT(param)
        map(lambda i: ort.update(X[i,:],y[i]), range(n))
        ort.draw()
        preds = map(lambda i: ort.predict(X[i,:]), range(n))
        acc = map(lambda z: z[0]==z[1] , zip(preds,y))
        print "Accuracy: " + str(sum(acc) / (n+1E-10))
        print "max depth: " + str(ort.tree.maxDepth())
        #print "Root counts: " + str(vars(ort.tree.elem.stats))

if __name__=='__main__':
    unittest.main()
