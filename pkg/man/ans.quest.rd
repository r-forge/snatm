\name{ans.quest}
\alias{ans.quest}
\encoding{UTF-8}
\title{Calculate the number of answers and questions per person in a mailing list}
\description{Calculate the number of answers and questions per person in a mailing list.}
\usage{ans.quest(forest)}
\arguments{
\item{forest}{Result of \code{makeforest()}}
}
\value{A matrix of dimension n x 3 where n is the number of authors in \code{forest}.
First column contains author name, second column contains number of questions, third column contains number of answers.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}