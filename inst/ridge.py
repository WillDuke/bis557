import numpy as np
import random

def ridgeHelper(y, X, lambda_vals):
    '''ridge shrinkage via svd with support for multiple lambdas'''

    # svd: X = U * Sigma * VT
    U, d, V = np.linalg.svd(X, full_matrices = False)
    col = X.shape[1]
    k = len(lambda_vals)
    d = d.reshape(-1,1)
    # initialize array
    betas = np.empty((k,col))

    # loop through values of lambda
    for i in range(k):
      l_val = np.repeat(lambda_vals[i], len(d)).reshape(-1,1)
      # Dplus = np.diag(d/(d ** 2 + l_val))
      Dplus = np.diagflat(np.divide(d, np.add(np.power(d, 2), l_val)))
      # w =  V * Dplus * UT * Y
      betas[i,:] = V.T.dot(Dplus).dot(U.T).dot(y).reshape(1,-1)

    return betas

def mse(y, yhat):
    '''calculate mean squared error'''

    mean = np.mean((y - yhat) ** 2)
    
    return mean

def folds(X, k):
    '''create indices for k-fold cross validation'''
    
    # shuffle indices in X
    shuffled = list(range(X.shape[0]))
    random.shuffle(shuffled)
    
    # initialize test and train fold indices
    test_inds = [[] for i in range(k)]
    train_inds = test_inds.copy()
    
    # initialize counters, get n and fold-size
    i, j, h = 0, 0, 0
    n = X.shape[0]
    m = int(n/k)
    
    # partition test indices with uneven chunksize handling
    for ind in shuffled:
        test_inds[i].append(ind)
        j += 1
        if n - j > m*k:
            i -= 1
        elif not j % m:
            i += 1
    
    # save train indices as remaining rows in X for each fold (K - 1)
    for ind in test_inds:
        train_inds[h] = [g for g in shuffled if g not in test_inds[h]]
        h += 1

    return test_inds, train_inds

def crossval(y, X, lambda_vals = np.exp(np.linspace(-2,6,100)), k = 10):
    '''k-fold cross validation of ridge regression'''

    # create folds of rough size n/k
    tests, trains = folds(X, k)
    
    # initialize array of errors and loop counter
    errors = [[] for i in range(k)]
    i = 0

    # loop through testing and training sets
    for test, train in zip(tests, trains):

        betas = ridgeHelper(y[train], X[train, ], lambda_vals)
        
        # get mse from coefficients from each lambda
        for row in betas:

            yhat = X[test,] @ row.reshape(-1,1)
            err = mse(y[test], yhat)
            errors[i].append(err)

        i += 1
    
    means = [sum(col) / float(len(col)) for col in zip(*errors)]

    index = np.argmin(means)

    return lambda_vals[index]
