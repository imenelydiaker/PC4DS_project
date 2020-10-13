
## F(x) = 1 - (beta/x)^alpha (= y)
## F-1(x) => x = beta / (1 - y)^(1/alpha)

inv.f <- function(y, alpha, beta) (beta / (1 - y)^(1/alpha))

simu.X <- function(n_samples){
  alpha = 3; beta = 100000
  y = runif(n_samples, 0, 1)
  X = inv.f(y, alpha, beta)
  X
}

X = simu.X(n_samples = 1000)
print(X)

plot(density(X))
# plot(density(X), type = 'hist')

library(EnvStats)
pareto = rpareto(1000, 0, 1)
print(pareto)
