# this script
import os

import matplotlib.pyplot as plt
import numpy as np
from scipy import signal
import json


def get_lag(yours, golden):
    yours = np.hstack((yours, yours[:len(golden) - 1]))  # cyclic postfix
    correlation = signal.correlate(yours, golden, mode="valid")
    lags = signal.correlation_lags(yours.size, golden.size, mode="valid")  # get lags for the result of correlation
    lag = lags[np.argmax(correlation)]
    return lag  # latency, yours after golden


def get_corrcoef(yours, golden):
    coeffs = np.corrcoef(yours, golden)
    return coeffs[0, 1]


def export_signal(signals):
    np.savez("output.npz", signals)


def import_signal():
    data = np.load("input.npz")
    return data


def export_config(config_dict):
    with open("config.json", "w", encoding='utf-8') as f:
        json.dump(config_dict, f)


def import_config():
    with open("config.json", encoding='utf-8') as f:
        return json.load(f)


if __name__ == '__main__':  # test
    config_dict = {
        "name": "configs",
        "pulse_points": 50000,
        "gauge_points": 50.0,
        "data": [1, 2, 3, 4]
    }

    export_config(config_dict)
    print(type(import_config()["gauge_points"]))
    print(type(import_config()["pulse_points"]))
    print(type(import_config()["name"]))
    print(type(import_config()["data"]))
    print(type(import_config()["data"][0]))
