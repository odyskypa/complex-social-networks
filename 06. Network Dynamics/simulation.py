import time
import sys
import os

import matplotlib.pyplot as plt

from ba_pa import BA_PA
from ba_ra import BA_RA
from ba_pa_no_growth import BA_PA_NG

def plot(x1, y1, x2, y2, x3, y3):
    """
    Plot the degree distribution for each model
    """
    fig, axs = plt.subplots(3)
    fig.suptitle('Vertically stacked subplots')
    axs[0].scatter(x1, y1)
    axs[1].scatter(x2, y2)
    axs[2].scatter(x3, y3)
    plt.show()

if __name__ == "__main__":
    if len(sys.argv) < 5:
        print("usage: simulation.py {n0} {n0_no_growth} {m0} {tmax}")
        exit()

    n0 = int(sys.argv[1])
    no_nogrowth = int(sys.argv[2])
    m0 = int(sys.argv[3])
    tmax = int(sys.argv[4])
    verbose = False
    folder = f"data_{n0}_{no_nogrowth}_{m0}_{tmax}"
    os.makedirs(folder, exist_ok=True)

    pa = BA_PA(n0, m0, tmax) ## Simulate BA-PA
    start = time.time()
    pa.simulate(dirname=folder, filename="pa", verbose=verbose)
    end = time.time()
    elapsed_time = end - start
    print("pa elapsed time: {:.3f} seconds".format(elapsed_time))
    x1, y1 = pa.get_deg_distribution()
    del pa

    ra = BA_RA(n0, m0, tmax) ## Simulate BA-RA
    start = time.time()
    ra.simulate(dirname=folder, filename="ra", verbose=verbose)
    end = time.time()
    elapsed_time = end - start
    print("ra elapsed time: {:.3f} seconds".format(elapsed_time))
    x2, y2 = ra.get_deg_distribution()
    del ra

    pa_ng = BA_PA_NG(no_nogrowth, m0, tmax) ## Simulate BA-PA-NG
    start = time.time()
    pa_ng.simulate(dirname=folder, filename="pa_ng", verbose=verbose)
    end = time.time()
    elapsed_time = end - start
    print("pa_ng elapsed time: {:.3f} seconds".format(elapsed_time))
    x3, y3 = pa_ng.get_deg_distribution()
    del pa_ng
    
    plot(x1, y1, x2, y2, x3, y3)