# this script
import os

import matplotlib.pyplot as plt
import numpy as np
from scipy import signal


def get_lag(yours, golden):
    yours = np.hstack((yours, yours[:len(golden) - 1]))  # cyclic postfix
    correlation = signal.correlate(yours, golden, mode="valid")
    lags = signal.correlation_lags(yours.size, golden.size, mode="valid")  # get lags for the result of correlation
    lag = lags[np.argmax(correlation)]
    return lag  # latency, yours after golden


def get_corrcoef(yours, golden):
    coeffs = np.corrcoef(yours, golden)
    return coeffs[0, 1]
