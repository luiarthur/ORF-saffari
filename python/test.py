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
uspsTest = np.genfromtxt('../scala/src/test/resources/usps/test.csv', delimiter=' ')
y = np.array(uspsTrain[:,0],dtype=int)
X = uspsTrain[:,1:]
(n,k) = X.shape
param = {'numClass': np.unique(y).size, 'lam': 1, 'minSamples': 700, 'minGain': .01, 'numTest': 10, 'gamma': 0}

uspsORF = orf.ORF(param,orf.getRange(X))
i = 0
for row in uspsTrain:
    print i
    i += 1
    uspsORF.update(row[1:],row[0])

pred = np.zeros(len(uspsTest))
for i in range(len(pred)):
    pred[i] = uspsORF.predict(uspsTest[i,1:])

np.mean( pred ==  uspsTest[:,0])
print orf.confusion(pred,uspsTest[:,0])

# RF:
from sklearn.ensemble import RandomForestClassifier
rf = RandomForestClassifier(n_estimators=100,n_jobs=8)
for i in np.linspace(n/10,n,10):
    rf.fit(X[:i,:],y[:i])
    print "RF-Brieman: %02d%s" % (np.mean( rf.predict(uspsTest[:,1:]) ==  uspsTest[:,0] ) * 100, "%") 

