## File : test.R
## Description : * tests methods of insuranceRisk package
##              * answers question of Chap 22 Students projects section 22.4 Insurance risk
## Chapter 22, section 22.4

library(pkgInsuranceRisk)

## USEFUL FUNCTIONS ####

## Function to plot different simulations of assets during t years
plot.assets <- function(n = 20, FUN, t = 5){
  fortunes <- c(); go.bust <- list()
  for(i in seq(n)){
    asset <- FUN(t)
    go.bust <- append(go.bust, asset$go.bust)
    fortunes <- append(fortunes, asset)
    plot(0:5, asset$assets, type = 'l', xlab = 'years', ylab = 'assets')
    par(new = TRUE)
  }
  go.bust
}

## Function to calculate the probability of a company to go bust
bust.proba <- function(bust.logical){
  true <- Reduce("+", lapply(bust, sum))
  total <- length(bust)
  true / total
}

## Part 1 ####

X = simu_X(n_samples = 1000, alpha = 3, beta = 1e5)
print(X)

plot(density(X), type = 'hist')
hist(X)
par(new = TRUE)
plot(density(X))

library(EnvStats)
pareto = rpareto(n = 100,
                 location = 100000,
                 shape = 3)
plot(density(pareto), type = 'hist')
hist(pareto)
par(new = TRUE)
plot(density(pareto))

## Part 2 ####

## Test of z(t)
res <- z(5)
print(res)
plot(0:5, res$assets, type = 'l', xlab = 'years', ylab = 'assets')

## Plot company assets over 5 years
bust <- plot.assets(n = 20, FUN = z, t = 5)

## Probability of going bust
print(paste("Probability of going bust :", bust.proba(bust)))

## Part 3 ####

## Test of z_profit function
res2 <- z_profit(t = 5)
print(res2)
plot(0:5, res2$assets, type = "l", xlab = 'years', ylab = 'assets')

## Plot company assets with profit over 5 years
bust <- plot.assets(n = 20, FUN = z_profit, t = 5)

## Probability of going bust
print(paste("Probability of going bust :", bust.proba(bust)))


# Total profits taken over 5 years
fortunes <- z_profit(t = 5)
print(fortunes)
plot(0:5, fortunes$assets, type = "l", xlab = 'years', ylab = 'assets')
profits <- fortunes$profits
profits
