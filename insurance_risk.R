## F(x) = 1 - (beta/x)^alpha (= y)
## F-1(x) => x = beta / (1 - y)^(1/alpha)

## simulate size of claims per year
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


## assets at the end of 5 years
z <- function(t){
  premiums = 5500000; claims = simu.X(n_samples = 1)
  if(t == 0){ 
    return(1000000)
  }
  if(z(t-1) > 0){
    return(max(z(t-1) + premiums - claims, 0))
  }
  else return(0)
}

z(t = 5)
