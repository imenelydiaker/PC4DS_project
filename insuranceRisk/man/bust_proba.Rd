\name{bust_proba}
\alias{bust_proba}
\title{The probability of a company to go bust after a period of time}
\usage{
bust_proba(assets, profits)
}
\description{
Calculates the probability of a company to go bust after a period of time.


INPUT :

assets : list of assets

profits : boolean, TRUE if the list of assets contains profits FALSE if not, default value FALSE


OUTPUT :

numeric : probability of the company of going bust
}
\examples{
Profits of 5 years :

bust_proba(assets = tab, profits = FALSE)

[[1]]
[1] TRUE

[[2]]
[1] 1e+06

[[3]]
[1] 1000000.0  537209.6       0.0       0.0       0.0       0.0
}
