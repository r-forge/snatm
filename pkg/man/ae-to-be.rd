\name{ae-to-be}
\alias{ae-to-be}
\alias{protectwords}
\alias{Simple}
\alias{OxEnglish}
\alias{BritishEnglish}
\alias{Simpleplus}
\alias{zwords}
\alias{re_zwords}
\alias{unprotectwords}
\title{American English to British English}
\description{Transforms American English spelling into British English spelling.}
\usage{
Simpleplus(txt)
}
\arguments{
\item{txt}{A vector of character strings.}
}
\value{Returns \code{txt} in British English spelling.}
\source{\url{http://en.wikipedia.org/wiki/User:Ohconfucius/EngvarB.js}}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
\examples{
Simple("analyze")
}