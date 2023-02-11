import sys

import numpy as np
import matplotlib.pyplot as plt
from scipy import signal

def design_lowpass(tap, target, fs):
    bands = np.array((0, target - 0.4e6, target + 0.4e6, fs / 2))
    desired = (1, 1, 0, 0)  # low-pass

    fir_firls = signal.firls(tap, bands, desired, fs=fs)
    fir_remez = signal.remez(tap, bands, desired[::2], fs=fs)
    fir_firwin2 = signal.firwin2(tap, bands, desired, fs=fs)

    fig, axs = plt.subplots(nrows=2, ncols=1)
    hs = list()
    for fir in (fir_firls, fir_remez, fir_firwin2):
        freq, response = signal.freqz(fir)
        hs.append(axs[0].semilogy(0.5 * fs * freq / np.pi, np.abs(response))[0])
        axs[1].plot(0.5 * fs * freq / np.pi, np.angle(response))

    for band, gains in zip(zip(bands[:-1], bands[1:]),
                           zip(desired[:-1], desired[1:])):
        axs[0].semilogy(band, np.maximum(gains, 1e-7), 'k--', linewidth=2)

    axs[0].legend(hs, ('firls', 'remez', 'firwin2'), loc='lower center', frameon=False)
    axs[0].set_xlabel('Frequency (Hz)')
    axs[0].grid(True)
    axs[0].set(title='magnitude response tap = %d' % tap, ylabel='Magnitude')
    axs[1].set(title='phase response tap = %d' % tap, ylabel='Phase')

    fig.tight_layout()
    plt.savefig("filter response.png")

    np.savez("output.npz", fir_firwin2)
    print("view filter response.png for performance")
    print("load temp.npz for coeffs")

    return fir_firwin2


if __name__ == '__main__':
    if len(sys.argv) != 5:
        print("usage: design_filter tap target fs filter_type")
        design_lowpass(15, 80000000.0, 125000000.0)
    else:
        [tap, target, fs, filter_type] = sys.argv[1:]
        targets = target.strip('][').split(', ')
        if filter_type == "lowpass":
            design_lowpass(int(tap), float(targets[0]), float(fs))
