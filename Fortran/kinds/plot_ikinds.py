#/usr/bin/python3
import numpy as np
import matplotlib.pyplot as plt

ranges, kinds = np.loadtxt("ikinds.txt", dtype=np.int, unpack=True)

for kind in np.unique(kinds):
    plt.scatter(ranges[kinds == kind], kinds[kinds == kind], label="kind {}".format(kind), s=25)

plt.legend(loc="upper left", ncol=1, scatterpoints=1, framealpha=1.0)
plt.xlabel("exponent range")
plt.ylabel("integer kind")
plt.tight_layout()
plt.show()