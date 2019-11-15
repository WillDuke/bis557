import numpy as np

def soft_thresh(a, b):
    '''thresholding for lasso'''

    # send a to zero if abs(a) less than b
    if isinstance(a, float):
        if np.abs(a) <= b:
            a = 0
        if a > 0:
            a = a - b
        if a < 0:
            a = a + b
        return a
    else:
        raise("Non-float type.")

def update_beta(y, X, lambda_val, b, w):
    '''coordinate descent helper function that updates each coefficent once'''

    # create weights and Xb
    WX = X * w[:, np.newaxis] 
    WX2 = np.power(X, 2) * w[:, np.newaxis] 
    Xb = X @ b

    # update each coefficient with lasso penalty
    for i in range(len(b)):
        
        Xb = Xb - np.multiply(X[:,i].reshape(-1,1), b[i])
        
        b[i] = soft_thresh(np.sum(np.multiply(WX[:,i], \
            np.subtract(y, Xb).reshape(-1))), lambda_val)
        
        b[i] = np.divide(b[i], np.sum(WX2[ :,i]))
        Xb = Xb + np.outer(X[:,i],(b[i]))

    return b

def coord_descent(
    y, X, lambda_val, b = np.zeros((X.shape[1],1)), tol = 1e-5, \
    maxiter = 50, w = np.array(np.repeat(1, len(y))/len(y))):
    '''lasso model selection via coordinate descent'''

    # update until convergence or maxiter
    for i in range(maxiter):
        b_old = b.copy()
        b = update_beta(y, X, lambda_val, b, w)

        if np.all(np.abs(b) - np.abs(b_old) < tol):
            break

        if i == maxiter - 1:
            print("Function coord_descent did not converge.") 
    
    # pair values with variable keys and drop 0'd coefs
    keys = ["V" + str(num) for num in range(1,p + 1)]
    nonZeroBetas = {x:y for x,y in dict(zip(keys, b)).items() if y != 0}
    
    for key in nonZeroBetas.keys():
        nonZeroBetas[key] = round(float(nonZeroBetas[key]), 7)

    return nonZeroBetas
