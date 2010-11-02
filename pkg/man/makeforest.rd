\name{makeforest}
\alias{makeforest}
\encoding{UTF-8}
\title{Extract meta data and content from mailing lists}
\description{Extract meta data and content from mailing lists.}
\usage{makeforest(month)}
\arguments{
\item{month}{A character string containing the year and month to be analyzed in the form \code{"January-2008"}.}
}
\value{A matrix with a maximum of five columns.
First column contains the e-mail ID numbered consecutively in chronological order.
Second column contains the thread ID gained from \code{threads(tm.plugin.mail)}.
Third column contains the authors.
Fifth column contains the subject.
Sixth column contains the content.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
\examples{
#setInternet2(use = TRUE)
#download.file("https://stat.ethz.ch/pipermail/r-devel/2008-January.txt.gz", "2008-January.txt")
#forest <- makeforest("2008-January")
}