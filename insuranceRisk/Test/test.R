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
# print(res)
plot(0:5, res[[2]], type = 'l', xlab = 'years', ylab = 'assets')


## Plot company assets over 5 years
assets <- c()
for(i in seq(20)){
  par(new = TRUE)
  asset <- z(5)
  assets <- append(assets, asset)
  plot(0:5, asset[[2]], type = 'l', xlab = 'years', ylab = 'assets')
}
# print(assets)

print(paste("Probability of going bust :", bust_proba(assets = assets, profits = FALSE)))

## Part 3 ####

res2 <- z_profit(t = 5)
print(res2)
plot(0:5, res2[[3]], type = "l", xlab = 'years', ylab = 'assets')

## Plot company assets with profit over 5 years
assets <- c()
for(i in seq(20)){
  par(new = TRUE)
  asset <- z_profit(5)
  assets <- append(assets, asset)
  plot(0:5, asset[[3]], type = 'line', xlab = 'years', ylab = 'assets')
}
# print(assets)

print(paste("Probability of going bust :", bust_proba(assets = assets, profits = TRUE)))


# total profits taken during 5 years
assets <- z_profit(t = 5)
print(assets)
plot(0:5, assets[[3]], type = "l", xlab = 'years', ylab = 'assets')
profits <- assets[[2]]
profits


## Extra Part ####

## Optimized z function()
resultat <- z_optimized(5, TRUE)
print(resultat)
plot(0:5, resultat[[3]], type = "l", xlab = 'years', ylab = 'assets')

assets <- c()
for(i in seq(20)){
  par(new = TRUE)
  asset <- z_optimized(t = 5, profit = TRUE)
  assets <- append(assets, asset)
  plot(0:5, asset[[3]], type = 'line', xlab = 'years', ylab = 'assets')
}
# print(assets)

print(paste("Probability of going bust :", bust_proba(assets = assets, profits = TRUE)))
