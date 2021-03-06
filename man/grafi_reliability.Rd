% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance_tables.R
\name{grafi_reliability}
\alias{grafi_reliability}
\title{Table of reliability coeficientes for Mcdonald's Omega}
\usage{
grafi_reliability(x)
}
\arguments{
\item{x}{McDonalds Omega calculated with the omegaSem function. The name of the McDonald's Omega specified with the psych and GPArotation packages.}
}
\value{
The output is a data.frame that gives you fit McDonalds Omega coefficients in a table.
}
\description{
This function needs a psych and GPArotation object from the omegaSem function to work. It creates a table of all fit indices for a CFA and SEM model.
}
\examples{
set.seed(123456)
data <- data.frame(replicate(10,sample(1:5,1000,rep=TRUE)))
data.rel <- psych::omegaSem(data)

grafi_reliability(data.rel)
}
\author{
Juan Carlos Saravia
}
