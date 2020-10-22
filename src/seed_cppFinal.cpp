#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
using std::exp;

/**
*  File  : seed_dispersal_1.r
** Description:Seed Dispersal
** Description:Simulating the radial distance R
** By 
** Fonction de simulation de densité de la distance R par méthode de rejet ####
**  Input
*   f : Fonction de densité
*   n : Nombre d'observations
*   a : Borne supérieure de f
*   Output
*   r : Densité estimée

*/



// [[Rcpp::export]]
NumericVector r_dist_sim_cpp(Function f,int n, int a) {
  NumericVector ds;
  NumericVector du = runif(n, 0, a);
  NumericVector dt = f(n);
  for (int i = 0; i<=n; i++)
  {
    if( du[i] < dt[i]){
      ds.push_back(dt[i]); }
  }
  return ds;
}




/***  Fonction Utile ***/

/** Fonction de Distribution exponentielle **/
// [[Rcpp::export]]
NumericVector f_rexp_cpp(int n)
{
  return Rcpp::rexp(n, 0.5);
}

/** Fonction de Distribution log-normale **/
// [[Rcpp::export]]
NumericVector f_rlnorm_cpp(int n)
{
  return Rcpp::rlnorm(n, 0.5, 0.55);
}


/** Fonction de Distribution de Weibull **/
// [[Rcpp::export]]
NumericVector f_rweibull(int n)
{
  return Rcpp::rweibull(n, 2, 2);
}


/**Fonction de Distribution Chi-2 avec 3 degrés de liberté**/
// [[Rcpp::export]]
NumericVector f_Chi2(NumericVector x)
{ 
  return (1 / (2 * sqrt(3.14))) * (x*x) * exp(-(x*x) / 4);
}


/**Les fonctions d'affichage sont codés en R en bas 
 * et les constantes aussi
 */


/**FONCTION DE DENSITE

T = dt : Densité exponentielle **/
// [[Rcpp::export]]
NumericVector dt(NumericVector x) {
  NumericVector y  (x.length()) ; // y = vecteur de même longueur que x
  y = Rcpp::dexp(x, 0.5);
  return y;
}

/** R = dr : Densité gamma **/
// [[Rcpp::export]]
NumericVector dr(NumericVector x){
  NumericVector y (x.length());
  y = Rcpp::dgamma(x, 2, 0.5);
  return y;
}




/*** R

#quelques tests pour comparer le temps d'éxécution

#a)
#n = 5
#print("f_rexp version R")
#f_rexp <- function(n)
#rexp(n = n, rate = 1 / 2)
#print("son temps d'éxécution")
#system.time(f_rexp(n))
#print("celui de sa version cpp")
#f_rexp_cpp(n)
#system.time(f_rlnorm_cpp(n))

print("Test sur les histogrames")
n = 1000000
a = 20
# S = ds : Estimation de la distance radiale
ds = r_dist_sim_cpp(f_rexp_cpp,n,a);

#### PARTIE IMPLEMENTATION EN R

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

## CAS III : T~Weibull; R~Chi-2 ####

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

*/
