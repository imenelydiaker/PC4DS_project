\name{simu_X}
\alias{simu_X}
\title{Simulate the size of a typical claim}
\usage{
simu_X(n_samples, alpha, beta)
}
\description{
Simulates n samples of size of claims, generated following the distribution :

f(x) = { (alpha*beta)^alpha / (x + alpha)^(beta + 1) if x >= 0, else 0 }
given alpha and beta.

INPUT :

n_samples : number of samples to simulate

alpha : alpha constant

beta : beta constant

OUTPUT :

list : list of n samples
}
\examples{
Simulate 10 samples of claims, given alpha = 3 and beta = 100 000 :

simu_X(n_samples = 10, alpha = 3, beta = 1e5)

[1]  5077.722 26474.498 15340.466 63913.029 32210.281 21619.766   375.163  5507.225  8295.742 32738.137
}
