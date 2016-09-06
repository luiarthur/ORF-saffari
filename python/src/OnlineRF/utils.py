from math import log

def dataRange(X):
    def col(j):
        return map(lambda x: x[j], X)

    k = len(X[0]) # number of columns in X
    return map(lambda j: [ min(col(j)), max(col(j)) ], range(k))
    
def argmax(x):
    return x.index(max(x))

def log2(x):
    return log(x) / log(2)
