import numpy as np
import csv

def crossprods(yX_subset, chunksize, ycol, row):
    # convert to array
    chunk = np.array(yX_subset.reshape(-1, len(row)))
    # separate y and X
    y_chunk = chunk[:, ycol]
    X_chunk = np.array(np.delete(chunk, ycol, axis=1))

    # add intercept
    intercept = np.repeat(1,y_chunk.shape).reshape(-1,1)
    X_chunk = np.hstack((intercept, X_chunk))

    # compute xtx and xty for chunk
    xtx = X_chunk.T @ X_chunk
    xty = (X_chunk.T @ y_chunk).reshape(-1,1)

    return xtx, xty

def batch_ols(file, chunksize = 15, ycol = 0):

    with open(file, 'r') as csvfile:
        csvreader = csv.reader(csvfile, quoting=csv.QUOTE_NONNUMERIC)
        yX_subset, whole_xtx, whole_xty = [],[],[]
        i = 0
        for row in csvreader:
            yX_subset = np.append(yX_subset, row)
            i += 1

            if i % chunksize == 0:
                # find xtx and xty
                xtx, xty = crossprods(yX_subset, chunksize, ycol, row)

                #iteratively sum whole xtx and xty arrays
                if len(whole_xtx) == 0:
                    whole_xtx = xtx.copy()
                else: whole_xtx = np.add(whole_xtx, xtx.copy())
                if len(whole_xty) == 0:
                    whole_xty = xty.copy()
                else: whole_xty = np.add(whole_xty, xty.copy())

                # reset subset
                yX_subset = []


        # add last matrices if csv.shape[0] % chunksize != 0        
        if len(yX_subset) != 0:
            xtx, xty = crossprods(yX_subset, chunksize, ycol, row)
            whole_xtx = np.add(whole_xtx, xtx.copy())
            whole_xty = np.add(whole_xty, xty.copy())

    coef = np.linalg.solve(whole_xtx, whole_xty)

    keys = ["V" + str(num) for num in range(0, coef.shape[0])]
    nonZeroBetas = {x:y for x,y in dict(zip(keys, coef)).items() if y != 0}
    
    for key in nonZeroBetas.keys():
        nonZeroBetas[key] = round(float(nonZeroBetas[key]),4)

    return nonZeroBetas
