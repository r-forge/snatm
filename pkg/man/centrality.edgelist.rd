\name{centrality.edgelist}
\alias{centrality.edgelist}
\encoding{UTF-8}
\title{Create a valued edgelist for 2-mode-network of people and words}
\description{Create a valued edgelist for 2-mode-network of people and words.}
\usage{centrality.edgelist(terms,apply.to,list)}
\arguments{
\item{terms}{A character vector containing terms for which \code{extract.commnet} stored a network with file name
\code{net_terms}.}
\item{apply.to}{Whether the function should be applied to the terms in \code{subjects} or \code{content}.}
\item{list}{\code{rhelp} or \code{rdevel}}
}
\value{A matrix with three columns. First colum contains authors, second column contains terms and third column contains
line values representing the normalized degree rank of the author in the term's network.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
\examples{
# Suppose net is the network of all people who used "hallo" in the content of the rhelp mailing list.
net <- matrix(c(0,2,0,1,0,1,0,3,0),nrow=3)
rownames(net) <- colnames(net) <- c("PersonA","PersonB")
dir.create("rhelp")
dir.create("rhelp/content")
save(net,file="rhelp/content/net_hallo.rda")
centrality.edgelist("hallo",apply.to="content",list="help")
}