import numpy as np

def ridge(y, X, lambda_val):
    # svd: X = U * Sigma * VT
    U, d, V = np.linalg.svd(X, full_matrices = False)
    d = d.reshape(-1,1)

    l_val = np.repeat(lambda_val, len(d)).reshape(-1,1)

    Dplus = np.diagflat(np.divide(d, np.add(np.power(d, 2), l_val)))
    # w =  V * Dplus * UT * Y
    beta = V.T.dot(Dplus).dot(U.T).dot(y)

    return beta
    
def ridgeHelper(y, X, lambda_vals):

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

def rmse(y, yhat):

    # calculate root mean squared error
    mean = np.mean((y - yhat) ** 2) ** 0.5
    return mean

def crossval(y, X, lambda_vals = np.exp(np.linspace(-2,6,100))):

    # find betas with each lambda
    betas = ridgeHelper(y, X, lambda_vals)

    # initialize list
    errors = []

    # find error for each set of betas
    for row in betas:
        yhat = X @ row.reshape(-1,1)
        err = rmse(y, yhat)
        errors.append(err)

    index = np.argmin(errors)

    return lambda_vals[index]
