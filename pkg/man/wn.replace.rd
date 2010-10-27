\name{wn.replace}
\alias{wn.replace}
\encoding{UTF-8}
\title{Replace synonyms}
\description{Replaces terms by their synonyms, if these synonyms were also found in the text.}
\usage{wn.replace(terms,protect=NULL)}
\arguments{
\item{terms}{A character vector of terms.}
\item{protect}{A numerical vector indicating the index of terms in \code{wn.replace(terms)$replacements[,1]} that should not be replaced.}
}
\value{
\item{$terms}{Returns the input text with replaced synonyms.}
\item{$replacements}{A matrix with two columns, where the first column contains a term for which a synonym was found in the same text
and the second column contains the synonym found.}
}
\details{Should be performed twice if some terms should not be replaced by synonyms or by a certain synonym.
First time with \code{protect=NULL}.
Second time with \code{protect} being a numerical vector indicating the index of terms that should not be replaced.\\
Uses the \pkg{wordnet} package to find synonyms.}
\author{Ingo Feinerer and Angela Bohn \email{angela.bohn at gmail.com}}
\examples{
wn.replace(c("car","auto"))
wn.replace(c("car","auto"),protect=1)
}