\name{prepare.text}
\alias{prepare.text}
\title{Prepare Text and Calculate Term Frequencies}
\description{Perform a number of data preparation steps on a text corpus and calculate term frequencies.}
\usage{prepare.text(forest, terms.from = c("subjects", "content"),list=c("devel", "help"), protect = NULL, ae.to.be = TRUE,
                 replace = TRUE, stem = TRUE)}
\arguments{
\item{forest}{A matrix with five columns. Result of \code{makeforest}.}
\item{terms.from}{Should the function be applied on \code{subjects} or \code{content}?}
\item{list}{Should the function be applied on the \code{help} or \code{devel} mailing list?}
\item{ae.to.be}{Logical. Should American English spelling be transformed to British English spelling? Defaults to \code{TRUE}.}
\item{replace}{Logical. Should terms be replaced by synonyms found in the text. See also \code{?wn.replace}}
\item{protect}{A numerical vector indicating the index of terms that should not be replaced by synonyms.}
\item{stem}{Logical. Should terms be stemmed using the \pkg{tm} Snowball stemmer? Defaults to \code{TRUE}.}
}
\value{
\item{$termfreq}{A named vector containing the term frequencies of terms found in \code{forest} after a number of preparation steps.}
\item{$forest}{Returns \code{forest} after preparation.}
}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
