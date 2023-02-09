import sys
import os
import matplotlib.pyplot as plt
import numpy as np
import funcs
def corr_metric():
    npz_path = "./temp.npz"
    npz = np.load(npz_path)
    yours = npz["arr_0"]
    golden = npz["arr_1"]

    lag = funcs.get_lag(yours, golden)

    # TODO: make this a function
    if lag < 0:
        your_valid = yours[:-lag]
        golden_valid = golden[lag:]
    elif lag == 0:
        your_valid = yours
        golden_valid = golden
    else:
        your_valid = yours[lag:]
        golden_valid = golden[:-lag]

    ret = funcs.get_corrcoef(your_valid, golden_valid)

    fig, ax = plt.subplots()
    ax.plot(your_valid)
    ax.plot(golden_valid)
    ax.legend(['yours', 'golden'])
    fig.savefig("./corr.png")

    print(ret, lag)  # return corrcoef and lag by output


if __name__ == '__main__':
    if len(sys.argv) != 1:
        print("usage: corr_metric")
    else:
        corr_metric()
