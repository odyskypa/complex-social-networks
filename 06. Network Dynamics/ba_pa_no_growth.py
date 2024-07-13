from ba import BA
import numpy as np
from random_int_generator import RandomInt

class BA_PA_NG(BA):
    def __init__(self, n0, m0, tmax):
        super().__init__(n0, m0, tmax)
        self.ni_t1 = None
        self.ni_t10 = None
        self.ni_t100 = None
        self.ni_t1000 = None
    
    def simulate(self, dirname, filename, arrival_time = [1, 10, 100, 1000], verbose = False):
        super().initial_step()
        super().simulate(dirname, filename, arrival_time, verbose)

    def step(self, t):
        initial_node = np.random.randint(0, self.n)
        if t == 1:
            self.ni_t1 = initial_node
        elif t == 10:
            self.ni_t10 = initial_node
        elif t == 100:
            self.ni_t100 = initial_node
        elif t == 1000:
            self.ni_t1000 = initial_node
        nodes = list(self.deg.keys())
        nodes.remove(initial_node)
        proba = [self.deg[node]/(self.deg_sum) for node in nodes if node != initial_node]
        self.int_choices_generator = RandomInt(nodes=nodes, proba=proba, m =self.n-1)
        choices = self.int_choices_generator.generate_unique_choices()
        added = 0
        for node in choices:
            if node not in self.neighbors[initial_node]:
                self.deg[node] += 1
                self.deg[initial_node] += 1
                self.deg_sum += 2
                self.neighbors[node].append(initial_node)
                self.neighbors[initial_node].append(node)
                added += 1
                if added == self.m:
                    break
        for node, deg in self.deg.items():
            self.node_timeseries[node].append(deg)