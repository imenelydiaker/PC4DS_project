## File  : seed_dispersal_1.r
## Description:Seed Dispersal
## Description:Simulating the radial distance R
## By 
## Fonction de simulation de densité de la distance R par méthode de rejet ####
# Input
#   f : Fonction de densité
#   n : Nombre d'observations
#   a : Borne supérieure de f
# Output
#   r : Densité estimée
r_dist_sim <- function(f, n, a) {
  du <- runif(n, min = 0, max = a)
  dt <- f
  ds <- dt[du < dt]
  return(ds)
}

## Fonction Utile ####

## Fonction de Distribution exponentielle
f_rexp <- function(n)
  rexp(n = n, rate = 1 / 2)

## Fonction de Distribution log-normale 
f_rlnorm <- function(n)
  rlnorm(n = n, meanlog = 0.5, sdlog = 0.55)

## Fonction de Distribution de Weibull
f_rweibull <- function(n)
  rweibull(n = n, shape = 2, scale = 2)

## Fonction de Distribution Chi-2 avec 3 degrés de liberté
f_Chi2 <-
  function(x)
    (1 / (2 * sqrt(pi))) * x ^ 2 * exp(-(x ^ 2) / 4)

## Fonction d'affichage d'histogramme
draw_hist <- function(S, title, seq_by, xlim_max) {
  hist(
    S,
    breaks = seq(0, max(S) + 0.5, seq_by),
    freq = FALSE,
    xlim = c(0, xlim_max),
    ylim = c(0, dexp(0, rate = 1 / 2)),
    main = title,
    xlab = "r",
    ylab = expression(paste(f[R](r), " et ", hat(f)[S](r))),
    col = "lightgrey",
    border = "darkgrey"
  )
}


## Constantes ####
# Nombre d'observations
n <- 1000000
# La valeur de la borne supérieure
a <- 20
## CAS I : T~exp; R~gamma  ####

# T = dt : Densité exponentielle
dt <- function(x)
  dexp(x, rate = 1 / 2)

# R = dr : Densité gamma
dr <- function(x)
  dgamma(x, shape = 2, rate = 1 / 2)

# S = ds : Estimation de la distance radiale
ds <- r_dist_sim(f_rexp(n), n, a)

## Affichage des graphes CAS I ####

# Affichage de graphes côte à côte
par(las = 1,
    mfrow = c(1, 2),
    mar = c(4, 5, 3, 2))

# Affichage de la densité de R et T sur la figure gauche
curve(
  dr(x),
  from = 0,
  to = 20,
  ylim = c(0, dt(0)),
  lty = 2,
  xlab = "r",
  ylab = expression(paste(dt(r), " et ", dr(r)))
)
curve(dt(x), add = TRUE)
abline(h = 0, col = "grey")

## affichage de la densité estimée de S et R sur la figure droite
draw_hist(ds,
          title = "S" ,
          seq_by = 0.5,
          xlim_max = 20)
curve(dr(x), add = TRUE)

## CAS II : T~lognorm; R~lognorm ####

# Estimation de S
ds <- r_dist_sim(f_rlnorm(n), n, a)

## CAS II : T~Weibull; R~Chi-2 ####

# Estimation de S
ds <- r_dist_sim(f_rweibull(n), n, a)

## Affichage des graphes des cas II et cas III ####
# Affichage côte à côte
par(las = 1,
    mfrow = c(1, 2),
    mar = c(4, 5, 3, 2))

# Affichage de lhistogramme de S et les densités de T et R sur le graphe à gauche
draw_hist(ds,
          title = "Lognormal",
          seq_by = 0.125 ,
          xlim_max = 7)
curve(dlnorm(x, meanlog = 0.5, sdlog = 0.55),
      add = TRUE,
      lty = 2)
curve(dlnorm(x, meanlog = 0.8025, sdlog = 0.55), add = TRUE)

# Affichage de l'histogramme de S et les densités de T et R sur le graphe à gauche
draw_hist(ds,
          title = "Weibull",
          seq_by = 0.125,
          xlim_max = 7)
curve(dweibull(x, shape = 2, scale = 2), add = TRUE, lty = 2)
curve(f_Chi2(x), add = TRUE)
