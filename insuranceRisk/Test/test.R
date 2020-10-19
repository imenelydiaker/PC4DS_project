## File : test.R
## Description : * tests methods of insuranceRisk package
##              * answers question of Chap 22 Students projects section 22.4 Insurance risk

library(insuranceRisk)

## Part 1 ####

X = simu_X(n_samples = 1000, alpha = 3, beta = 1e5)
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
par()
hist(pareto)

## Part 2 ####

res <- z(5)
print(res)
plot(0:5, res$assets, type = 'l', xlab = 'years', ylab = 'assets')


## Plot company assets over 5 years
fortunes <- c(); go.bust <- list()
for(i in seq(20)){
  par(new = TRUE)
  asset <- z(5)
  go.bust <- append(go.bust, asset$go.bust)
  fortunes <- append(assets, asset)
  plot(0:5, asset$assets, type = 'l', xlab = 'years', ylab = 'assets')
}

true <- Reduce("+", lapply(go.bust, sum))
print(true)
total <- length(go.bust)
print(total)
print(paste("Probability of going bust :", true / total))

## Part 3 ####

res2 <- z_profit(t = 5)
print(res2)
plot(0:5, res2$assets, type = "l", xlab = 'years', ylab = 'assets')

## Plot company assets with profit over 5 years
fortunes <- c(); go.bust <- list()
for(i in seq(20)){
  par(new = TRUE)
  asset <- z_profit(5)
  go.bust <- append(go.bust, asset$go.bust)
  fortunes <- append(assets, asset)
  plot(0:5, asset$assets, type = 'line', xlab = 'years', ylab = 'assets')
}
# print(assets)

true <- Reduce("+", lapply(go.bust, sum))
print(true)
total <- length(go.bust)
print(total)
print(paste("Probability of going bust :", true / total))


# total profits taken during 5 years
fortunes <- z_profit(t = 5)
print(fortunes)
plot(0:5, fortunes$assets, type = "l", xlab = 'years', ylab = 'assets')
profits <- fortunes$profits
profits


## Extra Part ####

## Optimized z function()
resultat <- z_optimized(5, TRUE)
print(resultat)
plot(0:5, resultat$assets, type = "l", xlab = 'years', ylab = 'assets')

fortunes <- c()
for(i in seq(20)){
  par(new = TRUE)
  asset <- z_optimized(t = 5, profit = TRUE)
  go.bust <- append(go.bust, asset$go.bust)
  fortunes <- append(assets, asset)
  plot(0:5, asset$assets, type = 'line', xlab = 'years', ylab = 'assets')
}
# print(assets)

true <- Reduce("+", lapply(go.bust, sum))
total <- length(go.bust)
print(paste("Probability of going bust :", true / total))
