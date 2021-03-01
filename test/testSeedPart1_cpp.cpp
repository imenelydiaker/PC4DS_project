#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
using std::exp;

/**
 *  File  : seed_dispersal_1.r
 ** Description:Seed Dispersal
 ** Description:Simulating the radial distance R
 ** By
 ** CE FICHIER EST LA VERSION CPP DU FICHIER SEED
 ** Son but est d'améliorer la performance du code R, qui évite
 ** Ses avantages, il évite la redondance et permet
 ** une compilation rapide des données.
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




/***  Fonctions Utiles ***/

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
NumericVector f_rweibull_cpp(int n)
{
  return Rcpp::rweibull(n, 2.0, 2.0);
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
 T = dt : Densité exponentielle  **/

// [[Rcpp::export]]
NumericVector dexp_cpp(NumericVector x) {
  NumericVector y  (x.length()) ; // y = vecteur de même longueur que x
  y = Rcpp::dexp(x, 0.5);
  return y;
}



//[[Rcpp::export]]
NumericVector dlnorm_cpp(NumericVector x){
  NumericVector y (x.length());
  y = Rcpp::dlnorm(x, 0.8025, 0.55);
  return y;
}




/*** R


#quelques tests pour comparer le temps d'éxécution


r_dist_sim <- function(f, n, a) {
  du <- runif(n, min = 0, max = a)
  dt <- f
  ds <- dt[du < dt]
  return(ds)
}

# distribution d'une fonction exponentielle

f_rweibull <- function(n) rweibull(n = n, shape = 2, scale = 2)
#  Densité exponentielle
dt <- function(x) dexp(x, rate = 1 / 2)
# R = dr : Densité gamma
dr <- function(x) dgamma(x, shape = 2, rate = 1 / 2)
#  Densité d_weibull
d_weibull <-function(x) dweibull(x, shape = 2, scale = 2)
f_rexp <- function(n) rexp(n = n, rate = 1 / 2)

n = 1e6
a = 20
# Pour n  = 1e6, comparons le temps d'éxécution des fonctions
# f_rexp avec f_rexp_cpp, qui renvoient tous une distribution
# suivant une loi exponentielle

##### FAISONS QUELQUES COMPARAISONS ####

print("le temps d'excution de la version R est de ")
print(system.time(f_rexp(n)))
print("Tandis que celui de la version Cpp est de ")
print(system.time(f_rexp_cpp(n)))

### Une fonction essentielle codée en cpp et qui diminue considérabelement le temps
### est la fonction r_dist_sim_cpp
### Comparons la, avec la version R r_dist_sim

#print("la comparson entre f_rexp et f_rexp_cpp")
#install.packages('microbenchmark')
#library(microbenchmark)
#microbenchmark(r_dist_sim(f_rexp(n), n, a), r_dist_sim_cpp(f_rexp_cpp, n, a))

system.time(r_dist_sim(f_rexp(n), n, a))
system.time(r_dist_sim_cpp(f_rexp_cpp, n, a))


ds_lognorm <- r_dist_sim_cpp(f_rlnorm_cpp, n, a)
ds_weibull <- r_dist_sim(f_rweibull(n), n, a)
print("comparaison entre ds_lognorm qui est en cpp  et dsweibull qui est en R" )
system.time(draw_hist(ds_lognorm,title = "Lognormal",seq_by = 0.125 ,xlim_max = 7))
system.time(draw_hist(ds_weibull,title = "Weibull",seq_by = 0.125, xlim_max = 7))


print("Test sur les histogrames")
n = 1000000
a = 20
# S = ds : Estimation de la distance radiale

# S = ds : Estimation de la distance radiale en utilisant cpp



## Fonction d'affichage d'histogramme du cas I
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


ds_exponentielle_cpp = r_dist_sim_cpp(f_rexp_cpp,n,a)
ds_exponentielle_R = r_dist_sim(f_rexp(n),n,a)

print("comparaison  entre l'histogramme cpp et celle de R")
system.time(draw_hist(ds_exponentielle_cpp,
                      title = "" ,
                      seq_by = 0.5,
                      xlim_max = 20))
system.time(draw_hist(ds_exponentielle_cpp,
                      title = "" ,
                      seq_by = 0.5,
                      xlim_max = 20))

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
draw_hist(ds_exponentielle_cpp,
          title = "" ,
          seq_by = 0.5,
          xlim_max = 20)
curve(dr(x), add = TRUE)

####### FIN CAS 1 ########





###############  CAS II : T~lognorm; R~lognorm ###########
# Estimation de S
# cette simulation est rapide en tems d'éxécution
# car on utlisite une fonction cpp qui rappelle une
#autre fonction cpp.
ds_lognorm <- r_dist_sim_cpp(f_rlnorm_cpp, n, a)

############   CAS III : T~Weibull; R~Chi-2   #############
# Estimation de S




#ds_weibull0 <- r_dist_sim_cpp(f_rweibull_cpp, n, a)
ds_weibull <- r_dist_sim(f_rweibull(n), n, a)

#identical(ds_weibull, ds_weibull0)

## Affichage des graphes des cas II et cas III ####
# Affichage côte à côte

par(las = 1,
    mfrow = c(1, 2),
    mar = c(4, 5, 3, 2))

# Affichage de lhistogramme de S et les densités de T et R sur le graphe à gauche

draw_hist(ds_lognorm,
          title = "Lognormal",
          seq_by = 0.125 ,
          xlim_max = 7)
curve(dlnorm(x, meanlog = 0.5, sdlog = 0.55),
      add = TRUE,
      lty = 2)

curve(dlnorm_cpp(x), add = TRUE)

# Affichage de l'histogramme de S et les densités de T et R sur le graphe à gauche
draw_hist(ds_weibull,
          title = "Weibull",
          seq_by = 0.125,
          xlim_max = 7)
curve(d_weibull(x), add = TRUE, lty = 2)
curve(f_Chi2(x), add = TRUE)


#### testons les histogrammes avec la version cpp et non cpp

#system.time(draw_hist(ds_lognorm,
#  title = "Lognormal",
# seq_by = 0.125 ,
# xlim_max = 7))
#system.time()





*/
