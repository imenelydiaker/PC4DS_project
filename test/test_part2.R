## File : test.R
## Description : * tests methods of pckgPC4DS package for insurance risk problem
##              * answers question of Chap 22 Students projects section 22.4 Insurance risk
## Chapter 22, section 22.4

library(pkgPC4DS)

## USEFUL FUNCTIONS ####

## Function to plot different simulations of assets during t years
multi.assets <- function(n = 20, FUN, t = 5){
  fortunes <- c(); go.bust <- list();
  assets <- data.frame(c(logical(), as.list(rep(0, t + 1))))
  # assets data.frame description :
  # first column has logical value : 0 if the company is still trading, 1 if it foes bust
  # the rest of the columns contain assets from year 0 to year t
  for(i in seq(n)){
    asset <- FUN(t)
    fortunes <- rbind(fortunes, c(asset$go.bust, asset$assets))
  }
  fortunes
}

## Function to calculate the probability of a company to go bust
bust.proba <- function(bust.logical){
  true <- sum(bust.logical)
  total <- length(bust.logical)
  true / total
}

## Part 1 : simulate X ####
print("PART 1..")
readline(prompt = "Tap Enter to start PART 1")

# Simulate X with teh given density function
print("Simulate 1000 X samples...")
X = simu_X(n_samples = 1000, alpha = 3, beta = 1e5)
print(X)
par(mfrow = c(2,2))
plot(density(X), type = 'h', main = "X density")
hist(X, main = "X density")
par(new = TRUE)
plot(density(X))

# Pareto distribution
print("Pareto distribution")
library(EnvStats)
pareto = rpareto(n = 100,
                 location = 100000,
                 shape = 3)
plot(density(pareto), type = 'h', main = "Pareto density")
hist(pareto, main = "Pareto density")
par(new = TRUE)
plot(density(pareto))

readline(prompt = "Tap Enter to continue")

## Part 2 : simulate Z ####
print("PART 2...")
readline(prompt = "Tap Enter to start PART 2")
par(mfrow = c(1,1))

## Test of z(t)
print("Simulation of assets over 5 years (Z(t)) :")
res <- z(5)
print(res)
plot(0:5, res$assets, type = 'b', xlab = 'years', ylab = 'assets',
     main = "Simulation of assets over 5 years")

readline(prompt = "Tap Enter to continue")

## Plot company assets over 5 years
print("Simulation of multiple assets over 5 years (Z(t)) :")
t <- 5
res.multi <- multi.assets(n = 20, FUN = z, t = t)
print(res.multi)
# first column is logical value : 0 if the company is still trading, 1 if it foes bust
# the rest of the columns contain assets from year 0 to year t

col <- ncol(res.multi); row <- nrow(res.multi)
plot(0, 0, type = "n",
     xlim = c(0,t), ylim = c(0, max(res.multi)),
     xlab = "years", ylab = "assets",
     main = "Simulation of multiple assets over 5 years")
for(i in seq(row)){
  lines(0:t, res.multi[i,2:col], type = 'b')
}

readline(prompt = "Tap Enter to continue")

## Probability of going bust
print(paste("Probability of going bust :", bust.proba(res.multi[,1])))

readline(prompt = "Tap Enter to continue")

## Part 3 : simulate Z with profits ####
print("PART 3...")
readline(prompt = "Tap Enter to start PART 3")

## Test of z_profit function
print("Simulation of assets over 5 years with profits :")
res2 <- z_profit(t = 5)
print(res2)
plot(0:5, res2$assets, type = "b",
     xlab = 'years', ylab = 'assets',
     ylim = c(0, max(res2$assets)),
     main = "Simulation of assets over 5 years with profits")

readline(prompt = "Tap Enter to continue")

## Plot company assets with profit over 5 years
print("Simulation of multiple assets over 5 years with profits :")

res2.multi <- multi.assets(n = 20, FUN = z_profit, t = t)
print(res2.multi)

col <- ncol(res2.multi); row <- nrow(res2.multi)
plot(0, 0, type = "n",
     xlim = c(0,t), ylim = c(0, max(res2.multi)),
     xlab = "years", ylab = "assets",
     main = "Simulation of multiple assets over 5 years with profits")
for(i in seq(row)){
  lines(0:t, res2.multi[i,2:col], type = 'b')
}

readline(prompt = "Tap Enter to continue")

## Probability of going bust
print(paste("Probability of going bust :", bust.proba(res2.multi[,1])))

readline(prompt = "Tap Enter to continue")

# Total profits taken over 5 years
fortunes <- z_profit(t = 5)
print(fortunes)
plot(0:5, fortunes$assets, type = "b",
     xlab = 'years', ylab = 'assets',
     main = "Simulation of assets over 5 years with profits")
profits <- fortunes$profits
print(paste("Total of profits taken over 5 years : ", profits))
