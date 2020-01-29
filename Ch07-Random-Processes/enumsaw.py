import numpy as np

dx = [[1, 0], [0, 1], [-1, 0], [0, -1]]

N = 1
walks = np.zeros(N, )
x = np.zeros((N+1, 2), dtype=np.int)
dir = np.zeros(N, dtype=np.int)
L = np.zeros((2*N+1, 2*N+1), dtype=np.bool)

n = 0
L[x[n, 0], x[n, 1]] = True
dir[0] = 0

while True:
    if dir[n] == 4:
        L[x[n, 0], x[n, 1]] = False
        n -= 1
        if n < 0:
            break
    else:
        x[n+1] = x[n]+dx[dir[n]]
        dir[n] += 1
        
        if not L[x[n+1, 0], x[n+1, 1]]:
            if n < N-1:
                n += 1
                L[x[n, 0], x[n, 1]] = True
                dir[n] = 0
            else:
                print(x)

