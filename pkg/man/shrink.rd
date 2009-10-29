\name{shrink}
\alias{shrink}
\encoding{UTF-8}
\title{Shrink a 2-mode-network}
\description{Shrink a 2-mode-network into a 1-mode-network by omitting one node set (S) and connecting two nodes of the other set (R)
if they were both formally connected a certain nodes of S.}
\usage{shrink(network,by=c("row","column"),keep=NULL,values=c("sum","min","max"))}
\arguments{
\item{network}{An adjacency matrix representing a 2-mode-network (bipartite network).
A 2-mode-network has two sets of nodes where each node can only be connected to nodes of the other set.}
\item{by}{.}
\item{keep}{A numerical vector containing the node numbers to be kept (node numbers of one node set).}
\item{values}{How should line values be aggregated. \code{sum} takes the sum of the line values that formally connected
the omitted node with its two adjacent nodes. \code{min} takes the minimum and \code{max} takes the maximum.}
}
\value{A matrix of dimension \code{length(keep)} x \code{length(keep)} containing the new connections for the remaining node set.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
\examples{
twomode <- matrix(c(0,1,0,0,1,0,1,0,0,1,0,1,0,0,0,1),ncol=4)
rownames(twomode) <- colnames(twomode) <- c("S1","R1","S2","R2")
onemode <- shrink(twomode,by="row",keep=c(1,3),values="sum")
}