import os
import sys

import matplotlib.pyplot as plt
import numpy as np
from scipy import signal


def plot_spectrum(fs):
    npz_path = "input.npz"
    npz = np.load(npz_path)
    data = npz["arr_0"]
    data = np.real(data)

    dt = 1 / fs
    t = np.arange(0, len(data)) * dt

    fig, axs = plt.subplots(nrows=3, ncols=1)

    # 时域信号
    axs[0].set_title("Signal")
    axs[0].plot(t, data, color='C0')
    axs[0].set_xlabel("Time")
    axs[0].set_ylabel("Amplitude")

    # 线性坐标频谱
    axs[1].set_title("Magnitude Spectrum")
    spectrum, freqs, _ = axs[1].magnitude_spectrum(data, Fs=fs, color='C1')
    spec_points = len(spectrum)

    peaks, _ = signal.find_peaks(spectrum, height=np.max(spectrum) * 0.5, distance=spec_points / 100)
    df = fs / 2 / spec_points

    if spectrum[0] >= np.max(spectrum) * 0.5:
        peaks = np.hstack((np.array(0), peaks))

    axs[1].plot(peaks * df, spectrum[peaks], "x")
    print("peak freqs = " + str(freqs[peaks] / 1e6) + " MHz")

    # 对数坐标频谱
    axs[2].set_title("Log. Magnitude Spectrum")
    axs[2].magnitude_spectrum(data, Fs=fs, scale='dB', color='C2')

    fig.tight_layout()
    plt.show()


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("usage: plot_spectrum fs")
    else:
        plot_spectrum(float(sys.argv[1]))
