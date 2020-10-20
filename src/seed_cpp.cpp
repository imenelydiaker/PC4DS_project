#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
using std::exp;


/**
  Ici sera codé la fonctio r_dist_sim en Cpp
}
**/

//Fonction Utile

// Fonction de Distribution exponentielle
// [[Rcpp::export]]
NumericVector f_rexp_cpp(int n){
  return rexp( n, 1/2);
}

// Fonction de Distribution log-normale 
// [[Rcpp::export]]
NumericVector f_rlnorm_cpp(int n){
  return rlnorm(n, 0.5, 0.55);
}


// Fonction de Distribution de Weibull
// [[Rcpp::export]]
NumericVector f_rweibull(int n){
    return rweibull(n, 2, 2);
}

//Fonction de Distribution Chi-2 avec 3 degrés de liberté
// [[Rcpp::export]]
double f_Chi2(double x){ 
  return (1 / (2 * sqrt(3.14))) * (x*x) * exp(-(x*x) / 4);
}




/*** R

#quelques tests pour comparer le temps d'éxécution

#a)
n = 1e8
print("f_rexp version R")
f_rexp <- function(n)
  rexp(n = n, rate = 1 / 2)
print("son temps d'éxécution")
system.time(f_rexp(n))
print("celui de sa version cpp")
system.time(f_rexp_cpp(n))

# sur cet expemple, vous remarquerez que la
#version Cpp est 8 fois plus rappired 
# que la fonction f_rexp en R
system.time(f_rlnorm_cpp(n))



#Cette parti est toujours codée en R et n'a pas encore été changée.

*/
