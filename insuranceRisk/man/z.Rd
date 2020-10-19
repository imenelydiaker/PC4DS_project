\name{z}
\alias{z}
\title{Simulates assets of a company during a given number of years}
\usage{
z(t)
}
\description{
Simulates assets of a company during a period of time in years, according to the following funtion :

z(0) = 1 000 000; z(t) = { max(z(t-1) + premiums - claims, 0) if z(t-1) > 0, 0 else}

INPUT :

t : period of time in number of years


OUTPUT :

boolean : if the company went bust or not

list : list of assets from year 0 to year t
}
\examples{
Profits of 5 years :

z(t = 5)

[[1]]
[1] FALSE

[[2]]
[1] 1000000 1733086 2514099 4194886 5083856 4787837
}
