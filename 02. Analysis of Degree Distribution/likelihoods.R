################################################################################
################################################################################

# 3. A toy ensemble of distributions

# Extra necessary functions

# Define the harmonic function
harmonic <- function(k, gamma) {
  sum(1 / (1:k)^gamma)
}

# Setting up all minus log likelihood functions

# Zeta distr.
minus_log_likelihood_zeta <- function(gamma) {
  N * log(zeta(gamma)) + gamma * M_l
}

# Zeta distr. with gamma=2
minus_log_likelihood_zeta2 <- function() {
  N * log(((pi^2)/6)) + 2 * M_l
}

# Zeta right truncated distr.
minus_log_likelihood_right_truncated_zeta <- function(kMax, gamma) {
  gamma * M_l + N * log(harmonic(kMax, gamma))
}

# Displaced geometric distribution
minus_log_likelihood_disp_geom <- function(q){
  -(M-N)*log(1-q)-N*log(q)
}

# Displaced Poisson
minus_log_likelihood_poisson <- function(lambda) {
  C + N*(lambda + log(1-exp(-lambda)))-M*log(lambda)
}

# Altmann
minus_log_likelihood_altmann <- function(gamma, delta) {
  gamma*M_l +delta*M*log(exp(1)) +.5*gamma*delta*N^2*(N+1)*log(exp(1)) + 
    gamma^2*N*sum(log(1:N))
}

# CORRECT ALTMANN LOGLIKELIHOOD
# gamma*M_l +delta*M +(gamma^2)*N*log(factorial(N))
# + gamma*delta*N*log(factorial(N))
