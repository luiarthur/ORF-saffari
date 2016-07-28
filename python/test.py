import orf
import numpy as np
import random

iris = np.genfromtxt('../scala/src/test/resources/iris.csv', delimiter=',')
iris[:,4] -= 1
np.random.shuffle(iris)
X = iris[:,0:4]
y = np.array(iris[:,4],dtype=int)
param = {'numClass': np.unique(y).size, 'lam': 1, 'minSamples': 5, 'minGain': .1, 'numTest': 3, 'gamma': 0}
ort = orf.ORT(param, orf.getRange(X))
for row in iris:
    xx = row[0:4]
    yy = row[4]
    ort.update(xx,yy)

pred = np.zeros(len(X))
for i in range(len(X)):
    pred[i] = ort.predict(X[i,:])

np.mean(pred==y)

irisORF = orf.ORF(param,orf.getRange(X))
for row in iris:
    xx = row[0:4]
    yy = row[4]
    irisORF.update(xx,yy)

orfPred = np.zeros(len(X))
for i in range(len(X)):
    orfPred[i] = irisORF.predict(X[i,:])


np.mean( orfPred == y )

