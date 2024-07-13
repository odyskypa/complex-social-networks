import os
import matplotlib.pyplot as plt
from random_int_generator import RandomInt
import random
import numpy as np

class BA:
    def __init__(self, n0, m0, tmax):
        self.m = m0
        self.n0 = n0
        self.n = n0
        self.s0 = self.n0 * self.m
        self.deg_sum = 0
        self.tmax = tmax
        self.t = 0
        self.deg = dict([(i, 0) for i in range(0, self.n0)])
        self.neighbors = dict([(i, []) for i in range(0, self.n0)])
        self.node_timeseries = {i: [] for i in range(self.n0)}
    
    def simulate(self, dirname, filename, arrival_time = [1, 10, 100, 1000], verbose = False):
        """
        Simulate the growth of a network
        """
        for t in range(self.tmax):
            if verbose:
                self.print_step_data()
            self.step(t)
        if verbose:
            self.print_step_data()
        self.save_time_series(dirname, filename, arrival_time)
        self.save_deg_sequence(os.path.join(dirname, f"{filename}_ds_{t+1}.txt"))

    def print_step_data(self):
        """
        Print information about the current step
        """
        print(f"n={self.n}, sum(ki)={self.deg_sum}")

    def step(self, t):
        """
        Compute a step of the simulation
        """
        pass

    def save_time_series(self, dirname, filename, arrival_time):
        """
        Save time series in a file
        """
        if filename != 'pa_ng':
            for t in arrival_time:
                with open(os.path.join(dirname, f"{filename}_ts_{t}.txt"), "w") as f:
                    line = ', '.join(map(str, self.node_timeseries[t + self.n0 - 1]))
                    f.write(f"{line}\n")
        else:
            for t in arrival_time:
                if t == 1:
                    sel = self.ni_t1
                elif t == 10:
                    sel = self.ni_t10
                elif t == 100:
                    sel = self.ni_t100
                elif t == 1000:
                    sel = self.ni_t1000
                with open(os.path.join(dirname, f"{filename}_ts_{t}.txt"), "w") as f:
                    line = ', '.join(map(str, self.node_timeseries[sel]))
                    f.write(f"{line}\n")

    def save_deg_sequence(self, filename):
        """
        Save the degree sequence in a file
        """
        with open(filename, "w") as f:
            for item in self.deg.items():
                f.write(f"{item[1]}\n")

    def get_deg_distribution(self):
        """
        Compute the degree distribution
        """
        deg_seq = dict()
        degs = self.deg.values()
        for deg in degs:
            count = deg_seq.get(deg, None)
            if count == None:
                deg_seq[deg] = 1
            else:
                deg_seq[deg] += 1
        items = deg_seq.items()
        x = [i[0] for i in items]
        y = [i[1] for i in items]
        return x, y
    
    def print_deg_distribution(self):
        """
        Print the degree distribution
        """
        x,y = self.get_deg_distribution()
        plt.plot(x, y)
        plt.show()
    
    def initial_step(self):
        """
        Create the initial graph. The initial graph contain n0 nodes with a degree of m0. If n0 is odd, 1 node will have a degree of m0 + 1.
        The initial number of stub will be n0 * m0.
        """
        for _ in range(self.m):
            s = list(self.deg.keys())
            random.shuffle(s)
            if len(s) % 2 != 0:
                ok = False
                while not ok:
                    v = np.random.randint(1, self.n)
                    if v != s[len(s)-1]:
                        s.append(v)
                        ok = True
            for i in range(0, len(s), 2):
                self.deg[s[i]] += 1
                self.deg[s[i + 1]] += 1
                self.deg_sum += 2
                self.neighbors[s[i]].append(s[i + 1])
                self.neighbors[s[i + 1]].append(s[i])
