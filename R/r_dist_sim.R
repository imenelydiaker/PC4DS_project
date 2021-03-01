r_dist_sim <- function(f, n, a) {
  du <- runif(n, min = 0, max = a)
  dt <- f
  ds <- dt[du < dt]
  return(ds)
}
