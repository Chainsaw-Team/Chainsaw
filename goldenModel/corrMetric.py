# this script
import os

import matplotlib.pyplot as plt
import numpy as np
from scipy import signal
from os import path

npz_path = "./pair.npz"
npz = np.load(npz_path)
yours = npz["arr_0"]
golden = npz["arr_1"]

# example data
# from numpy.random import default_rng
# rng = default_rng()
# golden = rng.standard_normal(1000)
# yours = np.concatenate([rng.standard_normal(100), golden[0:-100]])

# get lag
correlation = signal.correlate(yours, golden, mode="full")
lags = signal.correlation_lags(yours.size, golden.size, mode="full")  # get lags for the result of correlation
lag = lags[np.argmax(correlation)]

# get corrcoef
if lag < 0:
    yourValid = yours[0:lag]
    goldenValid = golden[-lag:]
else:
    yourValid = yours[lag:]
    goldenValid = yours[lag:]

coeffs = np.corrcoef(yourValid, goldenValid)
ret = coeffs[0, 1]

# plotting
fig, ax = plt.subplots()
ax.plot(yourValid)
ax.plot(goldenValid)
ax.legend(['yours', 'golden'])
fig.savefig("./corr.png")

print(ret, lag)  # return corrcoef and lag by output
