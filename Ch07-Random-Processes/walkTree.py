import numpy as np

Nmax = 4

dir = np.zeros(Nmax)
count = np.zeros(Nmax)
n = 0

while True:
    if dir[n] == 4:
        n -= 1
        if n == 0:
            break
    else:
        count[n] += 1

        dir[n] += 1
        if n < Nmax-1:
            n += 1
            dir[n] = 0
        


