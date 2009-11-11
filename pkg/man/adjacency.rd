\name{adjacency}
\alias{adjacency}
\encoding{UTF-8}
\title{Create an adjacency matrix from an edgelist}
\description{Create an adjacency matrix from an edgelist.}
\usage{adjacency(edgelist,directed=TRUE,valued=TRUE,mode="multiple")}
\arguments{
\item{edgelist}{A matrix with a minimum of three columns. Result of \code{createedges}.}
\item{directed}{logical. \code{TRUE} (default) if the adjacency matrix should be directed (asymmetric) or
\code{FALSE} if it should be undirected (symmetric).}
\item{valued}{logical. \code{TRUE} (default) if the adjacency matrix should be valued or
\code{FALSE} if it should be unvalued (binary).}
\item{mode}{The way how line values should be aggregated.
\code{multiple} (default) if line values should represent multiple lines (e.g. number of e-mails) or
\code{addvalues} if values in the third column of \code{edgelist} should be summed up.}
}
\value{A matrix representation of a network. Row names and column names contain names of nodes.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
}