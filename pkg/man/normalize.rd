\name{normalize}
\alias{normalize}
\encoding{UTF-8}
\title{Normalize a numeric vector}
\description{Normalize a numeric vector such that all values lie between 0 and 1.
The minimum of the new vector is 0, the maximum is 1.}
\usage{normalize(x)}
\arguments{
\item{x}{A numeric vector.}
\value{A numeric vector. All values lie between 0 and 1, the maximum is 1, the minimum 0.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
\examples{
a <- c(1,2,3,4)
normalize(a)
}