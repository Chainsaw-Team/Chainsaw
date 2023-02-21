import sys

import numpy as np


def periodic_unwrap(period):
    npz_path = "input.npz"
    npz = np.load(npz_path)
    data = npz["arr_0"]
    time_space = data.reshape([int(len(data) / period), period])
    # print("input")
    # print(time_space)
    ret = np.unwrap(time_space, axis=0)
    # print("output")
    # print(ret)
    ret = ret.flatten()
    np.savez("output.npz", ret)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("usage: periodic_unwrap period")
    else:
        periodic_unwrap(int(sys.argv[1]))
