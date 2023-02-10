import os
import sys

import numpy as np
import matplotlib.pyplot as plt
from scipy import signal
import funcs


def add():
    npz_path = "../temp.npz"
    npz = np.load(npz_path)
    a = npz["arr_0"]
    b = npz["arr_1"]
    np.savez("temp.npz", a + b)


if __name__ == '__main__':
    add()
