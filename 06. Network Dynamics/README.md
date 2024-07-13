# Network Dynamics

Simulate different network growth models and analyse their properties from a statistical perspective: 
- [ba.py](./ba.py) implements the base class (`BA`) for the Barabasi-Albert (BA) model
- [ba_pa.py](./ba_pa.py) inherits from `ba.py` and completes the implementation of the basic BA model (with `preferential attachment` and `node growth`)
- [ba_ra.py](./ba_ra.py) inherits from `ba.py` and completes the implementation of the BA model with `random attachment` instead of `preferential attachment`
- [ba_pa_no_growth.py](./ba_pa_no_growth.py) inherits from `ba.py` and completes the implementation of the basic BA model with `suppressed node growth`
- For executing the simulations run the [simulations.py](./simulation.py) script with the following arguments:
    - ```
      python simulation.py 250 1000 2 10000
      ```
- To perform degree sequence and vertex growth analysis execute the `Rmd` notebook:
    - `network-dynamics.Rmd`
