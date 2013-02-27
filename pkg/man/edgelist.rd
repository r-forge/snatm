\name{edgelist}
\alias{edgelist}
\title{Create an adjacency matrix from an edgelist}
\description{Create an edgelist from an adjacency matrix.}
\usage{edgelist(adjmatrix)}
\arguments{
\item{adjmatrix}{An adjacency matrix (square).}
}
\value{An matrix with two (if binary) or three (if valued) columns having the sender in the first column,
the receiver in the second, and the line value in the third column.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
