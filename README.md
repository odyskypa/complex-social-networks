# Complex Social Networks
Projects of Complex and Social Networks (CSN) Course for the Master in Data Science Program of Universitat Politècnica de Catalunya (UPC)
***

This repository contains the proposed solutions for the projects of the `Complex Social Networks` course. Each project focuses on different aspects of network analysis, ranging from basic network models to advanced epidemic spreading simulations. Below is a detailed description of each assignment and the corresponding solutions.

## Table of Contents

1. [Introduction to igraph](./01.%20Introduction%20to%20igraph/introduction-to-igraph)
2. [Analysis of the Degree Distribution](./02.%20Analysis%20of%20Degree%20Distribution/)
3. [Significance of Network Metrics](./03.%20Significance%20of%20Network%20Metrics/)
4. [Non-linear Regression on Dependency Trees](./04.%20Non-linear%20Regression%20on%20Dependency%20Trees/)
5. [Finding and Assessing Community Structure](./05.%20Finding%20and%20Assessing%20Community%20Structure/)
6. [Network Dynamics](./06.%20Network%20Dynamics/)
7. [Epidemic Spreading over Networks](./07.%20Epidemic%20Spreading%20over%20Networks/)
8. [Simulation of SIS Model over Weighted Networks](./08.%20Simulation%20of%20SIS%20Model%20over%20Weighted%20Networks/)

---

## Introduction to igraph

### [Objectives](./01.%20Introduction%20to%20igraph/docs/intro-to-igraph-statement.pdf)
- Understand the significance of `Watts-Strogatz` and `Erdős-Rényi` models.
- Analyze the `clustering coefficient` and the `average shortest-path` as functions of the parameter \( p \) in the WS model.
- Analyze the `average shortest-path length` as a function of the `network size` in the ER model.

### Solution
- Implementation of `Watts-Strogatz` and `Erdős-Rényi` models using igraph.
- Calculation and visualization of `clustering coefficients` and `average shortest paths`.
- Comparative analysis and interpretation of results.

---

## Analysis of the Degree Distribution

### [Objectives](./02.%20Analysis%20of%20Degree%20Distribution/docs/degree-distribution-statement.pdf)
- Analyze `in-degree sequences of the global syntactic dependency network` for different languages.
- Examine the empirical in-degree distribution of several languages’ lexicons.
- Assess the `adequacy of various probability distributions` in modeling the observed in-degree distribution patterns using **Akaike information criterion (AIC)**.

### Solution
- Data preparation and in-degree sequence extraction for different languages.
- Application of **probability distributions**: `Displaced Poisson, Displaced geometric, Zeta (with and without truncation), and Altmann function`.
- Parameter estimation using **Maximum Likelihood Estimation (MLE)** and `AIC-based` model comparison.

---

## Significance of Network Metrics

### [Objectives](./03.%20Significance%20of%20Network%20Metrics/docs/significance-of-metrics-statement.pdf)
- Analyze the `significance of network metrics` using *global syntactic dependency trees* from various languages.
- Test **observed network metrics against null hypotheses** based on `Erdős-Rényi` graphs and `switching` models.
- Implement a `Monte Carlo` procedure with `500` samples for significance testing.

### Solution
- Calculation of global `clustering coefficients`.
- `Hypothesis testing` with null models.
- `Monte Carlo` simulations with parallel computing for significance estimation.

---

## Non-linear Regression on Dependency Trees

### [Objectives](./04.%20Non-linear%20Regression%20on%20Dependency%20Trees/docs/Non-linear-regression-on-dependency-trees-statement.pdf)
- Investigate how the second moment of a sentence’s degree \( \langle k^2 \rangle \) scales with the number of nodes in the sentence.
- Find the best fit from a set of `8` models (**power-law, exponential, logarithmic**, etc.).
- Analyze `AIC` values and `test residuals` for **homoskedasticity**.

### Solution
- Data extraction and calculation of \( \langle k^2 \rangle \).
- **Non-linear regression** with multiple models.
- Model selection based on `AIC` and `residual analysis`.

---

## Finding and Assessing Community Structure

### [Objectives](./05.%20Finding%20and%20Assessing%20Community%20Structure/docs/Finding-and-assessing-community-structure-statement.pdf)
- Explore and compare various `community detection` algorithms.
- Evaluate communities based on `significance scoring functions` and the `Jaccard` similarity metric.

### Solution
- Implementation of community detection algorithms: **edge betweenness, fastgreedy, label propagation, leading eigenvector, multilevel, optimal, and spinglass**.
- `Evaluation of community quality` based on internal and external connectivity metrics.

---

## Network Dynamics

### [Objectives](./06.%20Network%20Dynamics/docs/network-dynamics-statement.pdf)
- Simulate and analyze the `Barabási-Albert (BA)` model and its variants.
- Apply `curve-fitting` methods on **degree distribution over time**.

### Solution
- Simulation of the `BA` model and its variants.
- Curve-fitting using `Displaced Poisson`, `Displaced geometric`, `Zeta` (with various parameters), and `Right-truncated Zeta`.
- Analysis and interpretation of **dynamical principles**.

---

## Epidemic Spreading over Networks

### [Objectives](./07.%20Epidemic%20Spreading%20over%20Networks/docs/Simulation-of-SIS-model-over-networks-statement.pdf)
- Investigate disease spreading using the `Susceptible-Infective-Susceptible (SIS)` model.
- Simulate the dynamics of disease transmission **across various network types**.
- `Validate` simulations against `theoretical predictions of epidemic thresholds`.

### Solution
- Implementation of the `SIS` model on different network structures (**Erdos-Renyi, scale-free, small-world, complete graphs, and tree structures**).
- Analysis of `simulation parameters` (**recovery rate, infection rate, initial infected fraction**).
- Validation against **theoretical** epidemic thresholds.

---

## Simulation of SIS Model over Weighted Networks

### [Objectives](./08.%20Simulation%20of%20SIS%20Model%20over%20Weighted%20Networks/docs/proposal.pdf)
- **Advance** the `SIS` model to incorporate **weighted networks** for `sexual epidemic spread`.
- Estimate epidemiological variables and validate epidemic thresholds for weighted cases.

### Solution
- Development of a **weighted SIS model**.
- Simulation of epidemic spread using `weights generated from` **normal, power-law, and exponential distributions**.
- Analysis of basic reproductive ratios and epidemic thresholds.
