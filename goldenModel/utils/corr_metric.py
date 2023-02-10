import os
import sys

import numpy as np
import matplotlib.pyplot as plt
import funcs

def corr_metric():
    npz_path = "../temp.npz"
    npz = np.load(npz_path)
    yours = npz["arr_0"]
    golden = npz["arr_1"]

    if len(yours) != len(golden):
        print("length of yours != length of golden, do truncation")
        length = np.min([len(golden), len(yours)])
        yours = yours[:length]
        golden = golden[:length]

    lag = funcs.get_lag(yours, golden)

    # TODO: make following branches a function
    if lag < 0:
        your_valid = yours[:lag]
        golden_valid = golden[-lag:]
    elif lag == 0:
        your_valid = yours
        golden_valid = golden
    else:
        your_valid = yours[lag:]
        golden_valid = golden[:-lag]

    ret = funcs.get_corrcoef(your_valid, golden_valid)

    if ret < 0.9:
        fig, ax = plt.subplots()
        ax.plot(your_valid)
        ax.plot(golden_valid)
        ax.legend(['yours', 'golden'])
        fig.savefig("./corr.png")
        print("view corr.png for failed test")

    print(ret, lag)  # return corrcoef and lag by output

if __name__ == '__main__':
    if len(sys.argv) != 1:
        print("usage: corr_metric")
    else:
        corr_metric()
