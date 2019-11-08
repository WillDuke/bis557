import csv

# need to initialize batch

def batch_ols(file, chunksize = 10, ycol = 1):

    with open(file, 'r') as csvfile:
        csvreader = csv.reader(csvfile, quoting=csv.QUOTE_NONNUMERIC)
        yX_subset = []
        i = 0
        j = 0
        for row in csvreader:
            yX_subset = np.append(yX_subset, row)
            i += 1
            if i == chunksize:
                # create np array of subset
                yXarr = np.array(yX_subset.reshape(-1, len(row)))

                # separate y and X
                y_chunk = yXarr[:,ycol]
                X_chunk = np.array(np.delete(yXarr, ycol, axis=1))

                # compute xtx and xty for chunk
                xtx = X_chunk.T @ X_chunk
                xty = (X_chunk.T @ y_chunk).reshape(-1,1)

                # add the new xtx
                if j == 0:
                    whole_xtx = xtx.copy()

                else: whole_xtx = np.add(whole_xtx, xtx)

                if j == 0:
                    whole_xty = xty.copy()

                else: whole_xty = np.add(whole_xty, xty)

                # reset subset and row counter, increment chunk counter
                yX_subset = []
                i = 0
                j += 1
    coef = np.linalg.inv(whole_xtx) @ whole_xty
    return coef
            
batch_ols("yX.csv")
