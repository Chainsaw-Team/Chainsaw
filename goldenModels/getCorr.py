# import matplotlib library
import matplotlib.pyplot as plt
import numpy as np

# float lists for cross
# correlation
x=[11.37, 14.23, 16.3, 12.36,
   6.54, 4.23, 19.11, 12.13,
   19.91, 11.00]

y=[15.21, 12.23, 4.76, 9.89,
   8.96, 19.26, 12.24, 11.54,
   13.39, 18.96]

# Plot graph
fig = plt.figure()
ax1 = fig.add_subplot(211)

# cross correlation using
# xcorr() function
ax1.xcorr(x, y, usevlines=True,
          maxlags=5, normed=True,
          lw=2)
# adding grid to the graph
ax1.grid(True)
ax1.axhline(0, color='blue', lw=2)

# show final plotted graph
plt.show()
