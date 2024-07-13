import numpy as np
import random
from ba import BA
from random_int_generator import RandomInt

class BA_RA(BA):
    
    def simulate(self, dirname, filename, arrival_time = [1, 10, 100, 1000], verbose = False):
        super().initial_step()
        super().simulate(dirname, filename, arrival_time, verbose)
    
    def step(self, t):
        nodes = list(self.deg.keys())
        self.int_choices_generator = RandomInt(nodes=nodes, m =self.m)
        choices = self.int_choices_generator.generate_unique_choices()
        self.n += 1
        self.neighbors[self.n - 1] = []
        self.deg[self.n - 1] = self.m
        self.node_timeseries[self.n - 1] = [0] * len(self.node_timeseries[0])
        for node in choices:
            self.deg[node] += 1
            self.deg_sum += 2
            self.neighbors[node].append(self.n - 1)
            self.neighbors[self.n - 1].append(node)
        for node, deg in self.deg.items():
            self.node_timeseries[node].append(deg)
