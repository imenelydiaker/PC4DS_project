inv_f <- function(y, alpha, beta){
  stopifnot(alpha > 0 && beta > 0)
  (beta / (1 - y)^(1 / alpha) - beta)
}

simu_X <- function(n_samples, alpha, beta) {
  y = runif(n_samples, 0, 1)
  inv_f(y, alpha, beta)
}
