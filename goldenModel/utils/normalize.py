import numpy as np
import matplotlib.pyplot as plt
import scipy.signal as signal


def normalize(data, period, overflow_limit, underflow_limit):
    """
    Normalize data to [-1, 1) by maintaining a scaling factor
    :param data: data to be normalized
    :param period: scaling factor will be modified once a period
    :param overflow_limit: increment scaling factor when ratio of overflow > overflow_limit
    :param underflow_limit: decrement scaling factor when ratio of underflow > underflow_limit
    :return: normalized data
    """

    scaling_factor = 1
    overflow, underflow = 0, 0
    ret = []
    scaling_history = []

    for i, elem in enumerate(data):
        prod = elem * scaling_factor
        ret += [prod]
        if np.abs(prod) > 1:
            overflow += 1
        elif np.abs(prod) < 0.5:
            underflow += 1

        if i % period == 0:
            if overflow / period > overflow_limit:
                scaling_factor -= 0.01
            elif underflow / period > underflow_limit:
                scaling_factor += 0.01
            overflow, underflow = 0, 0
            scaling_history += [scaling_factor]

    fig, axs = plt.subplots(3, 1)
    axs[0].plot(data)
    axs[1].plot(ret)
    axs[2].plot(scaling_history)

    plt.show()

    return ret


if __name__ == '__main__':
    period = 10
    data = np.sin(np.arange(0, period * 1000)) * 2
    normalize(data, period, 0.1, 0.9)
