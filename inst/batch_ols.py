import numpy as np
import csv

def crossprods(chunk, chunksize, ycol):
    '''compute crossproducts with intercept for batch_ols'''
    # separate y and X
    y = chunk[:, ycol]
    X = np.delete(chunk, ycol, axis=1)

    # add intercept
    intercept = np.repeat(1,y.shape).reshape(-1,1)
    X = np.hstack((intercept, X))

    # compute xtx and xty for chunk
    xtx = X.T @ X
    xty = (X.T @ y).reshape(-1,1)

    return xtx, xty

def batch_ols(file, chunksize = 10, ycol = 0):
    '''compute OLS in chunks from a csv'''

    with open(file, 'r') as csvfile:
        csvreader = csv.reader(csvfile, quoting=csv.QUOTE_NONNUMERIC)
        chunk, XTX = [],[]
        i = 0
        for row in csvreader:
        
            # append rows to chunk
            if len(chunk) == 0:
                chunk = np.array(row).reshape(1, -1)
            else: 
                chunk = np.vstack((chunk, row))
            i += 1
            
            # operate on each chunk
            if i % chunksize == 0:
            
                # find xtx and xty
                xtx, xty = crossprods(chunk, chunksize, ycol)

                # iteratively sum whole xtx and xty arrays
                if len(XTX) == 0:
                    XTX = xtx.copy()
                    XTY = xty.copy()
                else: 
                    XTX = np.add(XTX, xtx)
                    XTY = np.add(XTY, xty)

                # reset subset
                chunk = []

        # add last matrices if csv.shape[0] % chunksize != 0        
        if len(chunk) != 0:
            xtx, xty = crossprods(chunk, chunksize, ycol)
            XTX = np.add(XTX, xtx)
            XTY = np.add(XTY, xty)

    coef = np.linalg.solve(XTX, XTY)

    keys = ["V" + str(num) for num in range(0, coef.shape[0])]
    nonZeroBetas = {x:y for x,y in dict(zip(keys, coef)).items() if y != 0}
    
    for key in nonZeroBetas.keys():
        nonZeroBetas[key] = round(float(nonZeroBetas[key]),4)

    return nonZeroBetas
             
batch_ols("yX_ols.csv") 
