import os
import sys

import numpy as np
import matplotlib.pyplot as plt
from scipy import signal
import funcs


def unwrap():
    npz_path = "input.npz"
    npz = np.load(npz_path)
    data = npz["arr_0"]
    np.savez("output.npz", np.unwrap(data))

if __name__ == '__main__':
    unwrap()
