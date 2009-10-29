\name{permutation}
\alias{permutation}
\encoding{UTF-8}
\title{Permute an adjacency matrix}
\description{Permute an adjacency matrix.}
\usage{permutation(network,namesequence)}
\arguments{
\item{network}{An adjacency matrix to be permuted. Has to have row names and column names (no \code{NA}s or \code{NULL}s allowed).}
\item{namesequence}{A character vector having the same entries as \code{rownames(network)} and \code{colnames(network)}, but in
a different order. \code{network} will be permuted in such a way that \code{rownames(network)} and \code{colnames(network)}
are identical to \code{namesequence}.
}
\value{A permuted adjacency matrix where \code{rownames(network)} and \code{colnames(network)} are identical to \code{namesequence}.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
\examples{
network <- matrix(c(0,1,0,1,0,1,0,1,0),ncol=3)
rownames(network) <- colnames(network) <- c("PersonA","PersonB","PersonC")
permutation(network,c("PersonB","PersonA","PersonC"))
}