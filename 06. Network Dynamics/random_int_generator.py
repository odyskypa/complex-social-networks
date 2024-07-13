import random
import numpy as np

class RandomInt:
    def __init__(self, nodes = None, n = None, proba = None, m = None):
        self.nodes = nodes
        self.n = n
        self.proba = proba
        self.m = m
        
        if self.proba is not None:
            if sum(self.proba) != 1:
                self.proba = np.array(self.proba) / sum(self.proba)

    def generate_unique_random_numbers(self):
        i = random.randint(0, self.n - 1)
        j = random.randint(0, self.n - 1)

        while i == j:
            # Regenerate j until it's different from i
            j = random.randint(0, self.n - 1)

        return i, j
        

    def generate_unique_choices(self):
        if self.m >= len(self.nodes):
            return self.nodes

        choices_indices = np.random.choice(self.nodes, size=self.m, replace=False, p=self.proba)

        return choices_indices.tolist()
    
    def generate_unique_random_choices(self):
        if self.m >= len(self.nodes):
            return self.nodes

        indices = random.sample(range(len(self.nodes)), self.m, replace=False)

        return indices.tolist()