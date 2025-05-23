% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/banzhaf.R
\name{banzhaf}
\alias{banzhaf}
\title{Banzhaf value}
\usage{
banzhaf(
  characteristic_func,
  method = "exact",
  n_rep = 10000,
  n_players = 0,
  replace = FALSE
)
}
\arguments{
\item{characteristic_func}{The valued function defined on the subsets of the
number of players.}

\item{method}{Method used to calculate the Banzhaf value. Valid methods are:
\code{exact} for the exact calculation or \code{appro} for approximated
polynomial calculation based on sampling.}

\item{n_rep}{Only used if \code{method} is \code{appro}. The number of
iterations to perform in the approximated calculation}

\item{n_players}{Only used if \code{characteristic_func} is a \code{function}.
The number of players in the game.}

\item{replace}{should sampling be with replacement?}
}
\value{
The Banzhaf value for each player
}
\description{
Calculate the Banzhaf value
}
\examples{
n <- 8
v <- function(coalition) {
if (length(coalition) > n/2) {
   return(1)
 } else {
   return(0)
 }
}
banzhaf(v, method = "exact", n_players = n)
banzhaf(v, method = "appro", n_rep = 2000, n_players = n, replace = TRUE)

v<-c(0,0,0,1,2,1,3)
banzhaf(v, method = "exact")
banzhaf(v, method = "appro", n_rep = 2000, replace = TRUE)

}
