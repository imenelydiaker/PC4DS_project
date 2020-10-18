## File : insurance_risk.R


## Part 1 ####

## F(x) = 1 - (beta/x)^alpha (= y)
## F-1(x) => x = [beta / (1 - y)^(1/alpha)] - beta

## simulate size of claims per year
inv.f <- function(y, alpha, beta) ((beta / (1 - y) ^ (1 / alpha)) - beta)

simu.X <- function(n_samples) {
  alpha = 3
  beta = 100000
  y = runif(n_samples, 0, 1)
  inv.f(y, alpha, beta)
}

X = simu.X(n_samples = 1000)
print(X)

plot(density(X))
plot(density(X), type = 'hist')
hist(X)

library(EnvStats)
pareto = rpareto(n = 100,
                 location = 100000,
                 shape = 3)
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
z <- function(t) {
  premiums = 5500000; z_array = c(1000000); go.bust <- FALSE
  for (i in 1:t) { # i : annee precedente car les tableaux en R commencent a 1 et z_array est deja initialise a z_0
    claims = simu.X(n_samples = 100)
    print(sum(claims))
    if (z_array[i] > 0)
      z_t = max(z_array[i] + premiums - sum(claims), 0)
    else{
      go.bust <- TRUE
      z_array = append(z_array, rep(0, 6 - i))
      break
      # when the company goes bust it stays there so we abort the loop
    }
    z_array = append(z_array, z_t)
  }
  list(go.bust, z_array)
}

res <- z(5)
# print(res)
plot(0:5, res[[2]], type = 'line')


## Plot company assets over 5 years
assets <- c()
for(i in seq(100)){
  par(new = TRUE)
  asset <- z(5)
  assets <- append(assets, asset)
  plot(0:5, asset[[2]], type = 'line', xlab = 'years', ylab = 'assets')
}
# print(assets)

## Estimate the probability of the company to go bust
bust.proba <- function(assets){
  count = 0
  for(i in 1:length(assets)){
    if(i %% 2 == 1 && assets[[i]] == TRUE){
      count = count + 1
    }
  }
  count / length(assets)
}

print(paste("Probability of going bust :" ,bust.proba(assets = assets)))

## Part 3 ####

# assets + profit
z.profit <- function(t) {
  premiums = 5500000; z_array = c(1000000); go.bust <- FALSE
  for (i in 1:t) { # i : annee precedente car les tableaux en R commencent a 1 et z_array est deja initialise a z_0
    claims = simu.X(n_samples = 100)
    print(sum(claims))
    if (z_array[i] > 0)
      z_t = max(z_array[i] + premiums - sum(claims), 0)
    else{
      z_t = 0
      go.bust <- TRUE
      z_array = append(z_array, rep(0, 6 - i))
      break
      # when the company goes bust it stays there so we abort the loop
    }
    if (z_t > 1e6)
      z_array = append(z_array, z_t - 1e6)
    else
      z_array = append(z_array, z_t)
  }
  list(go.bust, z_array)
}


res2 <- z.profit(t = 5)
# print(res2)
plot(0:5, res2[[2]], type = "line")

## Plot company assets with profit over 5 years
assets <- c()
for(i in seq(20)){
  par(new = TRUE)
  asset <- z.profit(5)
  assets <- append(assets, asset)
  print(asset[[2]])
  plot(0:5, asset[[2]], type = 'line', xlab = 'years', ylab = 'assets')
}
# print(assets)

print(paste("Probability of going bust :" ,bust.proba(assets = assets)))
