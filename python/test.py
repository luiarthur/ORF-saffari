import orf
import numpy as np
import random

# Iris:
iris = np.genfromtxt('../scala/src/test/resources/iris.csv', delimiter=',')
iris[:,4] -= 1
np.random.shuffle(iris)
X = iris[:,0:4]
y = np.array(iris[:,4],dtype=int)
param = {'numClass': np.unique(y).size, 'lam': 1, 'minSamples': 5, 'minGain': .1, 'numTest': 3, 'gamma': 0}

irisORF = orf.ORF(param,orf.getRange(X))
for row in iris:
    xx = row[0:4]
    yy = row[4]
    irisORF.update(xx,yy)

pred = np.zeros(len(X))
for i in range(len(X)):
    pred[i] = irisORF.predict(X[i,:])


np.mean( pred == y )

orf.confusion(pred,y)

# USPS:
uspsTrain = np.genfromtxt('../scala/src/test/resources/usps/train.csv', delimiter=' ')
y = np.array(uspsTrain[:,0],dtype=int)
X = uspsTrain[:,1:]
(n,k) = X.shape
param = {'numClass': np.unique(y).size, 'lam': 1, 'minSamples': 100, 'minGain': .1, 'numTest': 10, 'gamma': 0}

uspsORF = orf.ORF(param,orf.getRange(X))
for row in uspsTrain:
    uspsORF.update(row[1:],row[0])


