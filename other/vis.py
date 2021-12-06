import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

def sigmoid(m, s):
    # There's a ZeroDivisionError even though nothing is zero...
    return lambda x: 1/(1+(x/m)**s)

def gaussian(b, m):
    return lambda x: np.exp( (np.log(0.5)*(x-b)**2) / (m-b)**2 )

def linearL(min_, max_):
    def inner(x):
        if x < min_:
            return 0
        if x > max_:
            return 1
        return (x - min_)/(max_ - min_)
    return inner

def linearS(min_, max_):
    def inner(x):
        if x < min_:
            return 1
        if x > max_:
            return 0
        return (-x + max_)/(max_ - min_)
    return inner

x = np.linspace(0,1,100)

# y1 and y2 auto broadcasts, but not y3 and y4
y1 = sigmoid(0.5, -5)(x)
y2 = gaussian(1, 0.5)(x)
y3 = list(map(linearL(0.2, 0.8), x))
y4 = list(map(linearS(0.2, 0.8), x))

plt.plot(x, y1, label='sigmoid(0.5, -5)')
plt.plot(x, y2, label='gaussian(1, 0.5)')
plt.plot(x, y3, label='linearL(0.2, 0.8)')
plt.plot(x, y4, label='linearS(0.2, 0.8)')
plt.legend()
plt.xlabel('Value of raster cell')
plt.ylabel('Standardized value')
plt.savefig('vis.png')
#plt.show()
