import numpy as np

n = 10
p = 15
X = np.random.normal(0, 1, (n, p))

beta = np.append(np.array([3, 2, 1]), np.repeat(0, p - 3), 0).reshape(-1, 1)
y = np.add(X @ beta, np.random.normal(0, 0.1, (n, 1)))
