## File : insurance_risk.R 


## Part 1 ####

## F(x) = 1 - (beta/x)^alpha (= y)
## F-1(x) => x = beta / (1 - y)^(1/alpha)

## simulate size of claims per year
inv.f <- function(y, alpha, beta) (beta / (1 - y)^(1/alpha))

simu.X <- function(n_samples){
  alpha = 3; beta = 100000
  y = runif(n_samples, 0, 1)
  inv.f(y, alpha, beta)
}

X = simu.X(n_samples = 1000)
print(X)

plot(density(X))
plot(density(X), type = 'hist')
hist(X)

library(EnvStats)
pareto = rpareto(n = 100, location = 100000, shape = 3)
plot(density(pareto))
plot(density(pareto), type = 'hist')
hist(pareto)



## Part 2 ####

# recursive version : not completed
# z <- function(t){
#   premiums = 5500000; claims = list()
#   print(t)
#   if(t == 0){
#     return(1000000)
#   }
#   if(z(t - 1) > 0){
#     claims = c(claims, simu.X(n_samples = 100))
#     return(max(z(t - 1) + premiums - Reduce('+', claims), 0))
#   }
#   else if(z(t-1) == 0){
#     return(0)
#   }
# }

## assets at the end of 5 years
z <- function(t){
  premiums = 5500000; z_array = c(1000000)
  for(i in 1:t) { # i : année précédente car les tableaux en R commencent à 1 et z_array est déjà initialisé à z_0
    claims = simu.X(n_samples = 100)
    print(sum(claims))
    if(z_array[i] > 0) z_t = max(z_array[i] + premiums - sum(claims), 0)
    else z_t = 0
    z_array = append(z_array, z_t)
  }
  z_array
}

res <- z(5)
res

plot(0:5, res, type = 'line')


## Part 3 ####

# assets + profit
z.profit <- function(t){
  premiums = 5500000; z0 = 1e6; z.prev = 0; z_array = c(z_0)
  for(i in 1:t) {
    claims = simu.X(n_samples = 100)
    print(sum(claims))
    if(z_array[i] > 0) z_t = max(z_array[i] + premiums - sum(claims), 0)
    else z_t = 0
    if(z_t > 1e6) z_array = append(z_array, z_t - 1e6)
    else z_array = append(z_array, z_t)
  }
  z_array
}


res <- z.proft(t = 5)
print(res)

