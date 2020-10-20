\name{fun_util}
\alias{FUN_UTIL}
\title{Fonctions utiles pour la bonne execution du code principal}
\usage{Fonctions de distributions et Fonction d'affichage d'histogramme
a partir de fonctions existantes.

\description
##Fonction de Distribution exponentielle
f_rexp <- function(n)
  rexp(n = n, rate = 1 / 2)

Fonction de Distribution log-normale
f_rlnorm <- function(n)
  rlnorm(n = n, meanlog = 0.5, sdlog = 0.55)

## Fonction de Distribution de Weibull
f_rweibull <- function(n)
  rweibull(n = n, shape = 2, scale = 2)

## Fonction de Distribution Chi-2 avec 3 degres de liberte
f_Chi2 <- function(x)
  (1 / (2 * sqrt(pi))) * x ^ 2 * exp(-(x ^ 2) / 4)

## T = dt : Densite exponentielle
d_exp <- function(x)
  dexp(x, rate = 1 / 2)

## R = dr : Densite gamma
d_gamma <- function(x)
  dgamma(x, shape = 2, rate = 1 / 2)

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


}
