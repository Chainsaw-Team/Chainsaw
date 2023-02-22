import numpy as np


def fft():
    npz_path = "input.npz"
    npz = np.load(npz_path)
    data = npz["arr_0"]
    np.savez("output.npz", np.fft.fft(data))


if __name__ == '__main__':
    fft()
