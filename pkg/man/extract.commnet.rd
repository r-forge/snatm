\name{extract.commnet}
\alias{extract.commnet}
\title{Extract a subnetwork of a communication network}
\description{Extract a subnetwork of a communication network of all people who used a certain term in the subject or content.}
\usage{extract.commnet(forest,terms,apply.to,data.path)}
\arguments{
\item{forest}{A matrix with five columns. Result of \code{makeforest}.}
\item{terms}{A character vector containing the terms for which networks should be extracted.}
\item{apply.to}{Should the function be applied on \code{subjects} or \code{content}?}
\item{data.path}{Base path to store the inferred data in serialised form.}
}
\value{Saves an \code{.rda} file named \code{net_term} in the subdirectory \code{base.paths/commnet.terms/apply.to}}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
