\name{initiate.respond}
\alias{initiate.respond}
\title{Calculate the number of message initiations and responses per person in a mailing list}
\description{Calculate the number of message initiations and responses per person in a mailing list.}
\usage{initiate.respond(forest)}
\arguments{
\item{forest}{Result of \code{makeforest()}}
}
\value{A matrix of dimension n x 3 where n is the number of authors in \code{forest}.
First column contains author name, second column contains number of questions, third column contains number of answers.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
