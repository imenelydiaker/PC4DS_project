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
double f_Chi2(double x){ 
  return (1 / (2 * sqrt(3.14))) * (x*x) * exp(-(x*x) / 4);
}

/**Les fonctions d'affichage sont codés en R en bas **/



/** Constantes **/
/**Nombre d'observations**/
int n = 1000000 ;
int a = 20; //apparemment ca fonctionne pas l'affectation
/**La valeur de la borne supérieure**/


/**FONCTION DE DENSITE

T = dt : Densité exponentielle **/
// [[Rcpp::export]]
NumericVector dt(NumericVector x) {
  NumericVector y  (x.length()) ; // y = vecteur de la même long que x
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
n = 5
print("f_rexp version R")
f_rexp <- function(n)
  rexp(n = n, rate = 1 / 2)
print("son temps d'éxécution")
system.time(f_rexp(n))
print("celui de sa version cpp")
f_rexp_cpp(n)

system.time(f_rlnorm_cpp(n))

print("voyons voir")
n = 1000000
a = 20
ds = r_dist_sim_cpp(f_rexp_cpp,n,a);

#### PARTIE IMPLEMENTATION EN R

*/
