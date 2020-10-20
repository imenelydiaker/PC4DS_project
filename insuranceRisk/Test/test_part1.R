## File  : seed_dispersal_TEST.r
## Title : 21.4.1 Simulating the radial distance R


## Constantes ####
# Nombre d'observations
n <- 1000000
# La valeur de la borne supérieure
a <- 20


## CAS I : T~exp; R~gamma  ####

# S = ds : Estimation de la distance radiale
ds <- r_dist_sim(f_rexp(n), n, a)

## Affichage des graphes CAS I ####

# Affichage de graphes côte à côte
par(las = 1,
    mfrow = c(1, 2),
    mar = c(4, 5, 3, 2))

# Affichage de la densité de R et T sur la figure gauche
curve(
  d_gamma(x),
  from = 0,
  to = 20,
  ylim = c(0, dt(0)),
  lty = 2,
  xlab = "r",
  ylab = expression(paste(dt(r), " et ", dr(r)))
)
curve(d_exp(x), add = TRUE)
abline(h = 0, col = "grey")

## affichage de la densité estimée de S et R sur la figure droite
draw_hist(ds,
          title = "S" ,
          seq_by = 0.5,
          xlim_max = 20)
curve(dr(x), add = TRUE)

## CAS II : T~lognorm; R~lognorm ####

# Estimation de S
ds_lognorm <- r_dist_sim(f_rlnorm(n), n, a)

## CAS III : T~Weibull; R~Chi-2 ####

# Estimation de S
ds_rweibull <- r_dist_sim(f_rweibull(n), n, a)

## Affichage des graphes des cas II et cas III ####
# Affichage côte à côte
par(las = 1,
    mfrow = c(1, 2),
    mar = c(4, 5, 3, 2))

# Affichage de l'histogramme de S et les densités de T et R sur le graphe à gauche
draw_hist(ds_lognorm,
          title = "Lognormal",
          seq_by = 0.125 ,
          xlim_max = 7)
curve(dlnorm(x, meanlog = 0.5, sdlog = 0.55),
      add = TRUE,
      lty = 2)
curve(dlnorm(x, meanlog = 0.8025, sdlog = 0.55), add = TRUE)

# Affichage de l'histogramme de S et les densités de T et R sur le graphe à gauche
draw_hist(ds_rweibull,
          title = "Weibull",
          seq_by = 0.125,
          xlim_max = 7)
curve(dweibull(x, shape = 2, scale = 2), add = TRUE, lty = 2)
curve(f_Chi2(x), add = TRUE)
