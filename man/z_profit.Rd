\name{z_profit}
\alias{z_profit}
\title{Simulates assets of a company during a given number of years and calculates taken profits}
\usage{
z_profit(t)
}
\description{
Simulates assets of a company during a period of time in years, according to the following funtion :

z(0) = 1 000 000; z(t) = { max(z(t-1) + premiums - claims, 0) if z(t-1) > 0, 0 else}.

Calculates the profit taken over a period of time.


INPUT :

t : period of time in number of years


OUTPUT :

boolean : if the company went bust or not

numeric : sum of profits made during t years

list : list of assets from year 0 to year t
}
\examples{
Profits of 5 years :

z_profit(t = 5)

[[1]]
[1] TRUE

[[2]]
[1] 1e+06

[[3]]
[1] 1000000.0  537209.6       0.0       0.0       0.0       0.0
}
