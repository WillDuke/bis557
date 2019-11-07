import numpy as np

def soft_thresh(a, b):

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
    
    WX = X * w[:, np.newaxis] # WX - 100 x 500
    WX2 = np.power(X, 2) * w[:, np.newaxis] # WX2 - 100 x 500
    Xb = X @ b # XB - 100 x 1

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

    # update until convergence or maxint
    for i in range(maxiter):
        b_old = b.copy()
        b = update_beta(y, X, lambda_val, b, w)

        if np.all(np.abs(b) - np.abs(b_old) < tol):
            break

        if i == maxiter - 1:
            print("Function coord_descent did not converge.") 
    
    keys = ["V" + str(num) for num in range(1,p + 1)]
    nonZeroBetas = {x:y for x,y in dict(zip(keys, b)).items() if y != 0}
    
    for key in nonZeroBetas.keys():
        nonZeroBetas[key] = float(nonZeroBetas[key])

    return nonZeroBetas
