################################################################################
################################################################################

# 4. Estimation of the parameters

# TODO: a better initial value for the 2 zetas can be inferred from the slope of
# a linear regression in log-log scale

# MLE for zeta distr.
mle_zeta <- function() {
  mle(minus_log_likelihood_zeta,
      start = list(gamma = 2),
      method = "L-BFGS-B",
      lower = c(1.0000001)
  )
}

# MLE for zeta distr. with gamma = 2
mle_zeta2 <- function(){
  m2logL <- minus_log_likelihood_zeta2()
  AIC_zeta2 <- 2 * m2logL
  result <- list(coef = 2, m2logL = m2logL, AIC_zeta2=AIC_zeta2)
  return(result)
}


# MLE for right truncated zeta distr.
mle_right_truncated_zeta <- function() {
  mle(minus_log_likelihood_right_truncated_zeta,
      start = c(kMax = max_degree, gamma = 2),
      method = "L-BFGS-B",
      lower = c(kMax = max_degree, gamma = 1.0000001),
      upper = c(kMax = N, gamma = 100)
  )
}


# MLE for displaced geometric distribution
mle_disp_geom <- function() {
  mle(minus_log_likelihood_disp_geom,
      start = list(q = N_M),
      method = "L-BFGS-B",
      lower = c(N_M)
  )
}


mle_poisson <- function (){
  mle(minus_log_likelihood_poisson,
      start = list(lambda = M_N),
      method = "L-BFGS-B",
      lower = c(1.0000001)
  )
}
  # MLE for Atlmann
mle_altmann <- function (){
  mle(minus_log_likelihood_altmann,
      start = list(gamma = 0.1, delta = 0.1),
      method = "L-BFGS-B",
      lower = c(gamma = 000000.1, delta = 0.0000001)
  )
}
