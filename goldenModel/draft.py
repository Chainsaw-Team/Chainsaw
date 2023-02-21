import os
import sys

import numpy as np
import matplotlib.pyplot as plt
from scipy import signal

example = np.ones([4,4])
example[3,:] = 10
print(example)
print(np.unwrap(example, axis=0))
print(np.unwrap(example, axis=1))
