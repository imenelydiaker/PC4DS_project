#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
using std::exp;







//Fonction Utile

// Fonction de Distribution exponentielle
// [[Rcpp::export]]
NumericVector f_rexp_cpp(int n){
  return Rcpp::rexp(n, 0.5);
}

// Fonction de Distribution log-normale 
// [[Rcpp::export]]
NumericVector f_rlnorm_cpp(int n){
  return rlnorm(n, 0.5, 0.55);
}

// [[Rcpp::export]]
NumericVector r_dist_sim_cpp(Function f,int n, int a) {
  NumericVector ds;
  NumericVector du = runif(n, 0, a);
  NumericVector dt = f(n);
  for (int i = 0; i<=n; i++){
    if( du[i] < dt[i]){
      ds.push_back(dt[i]);
    }
    
  }
  return ds;
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
r_dist_sim_cpp(f_rexp_cpp,2,3)
#Cette parti est toujours codée en R et n'a pas encore été changée.

*/
