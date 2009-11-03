\name{find.aliases}
\alias{find.aliases}
\alias{normalizeauthors}
\alias{sortnames}
\alias{emailfirst}
\alias{changenames}
\alias{findclusters}
\alias{final}
\encoding{UTF-8}
\title{Find e-mail and real name aliases}
\description{Find e-mail and real name aliases and replace them in result of \code{makeforest}.}
\usage{
normalizeauthors(authors)
sortnames(x)
emailfirst(y)
changenames(clusters,forest,accept=1:length(take.memory))
findclusters(v,distance=0.3,not.take.memory)
final(d)
}
\arguments{
\item{authors}{A character vector of author names including e-mail adresses (third column of result of \code{makeforest}).}
\item{x}{A character vector (result of \code{normalizeauthors}.}
\item{y}{A character vector (result of \code{sortnames}.}
\item{clusters}{A list. The first element of each list element contains the matched name and 
      the following elements contain the aliases found.}
\item{z}{A character vector (result of \code{emailfirst}).}
\item{accept}{A numeric vector containing the numbers of accepted list elements of \code{clusters}}
\item{v}{A character vector (result of \code{changenames}).}
\item{distance}{Numeric. Distance to be used for \code{base::agrep}. Defaults to 0.3.}
\item{not.take.memory}{A list. The first element of each list element contains the matched name and 
    the following elements contain aliases that are not correct.}
\item{d}{A character vector (result of \code{changenames)}.}
}
\value{
\item{normalizeauthors}{A character vector.}
\item{sortnames}{A character vector.}
\item{emailfirst}{A character vector.}
\item{changenames}{A character vector.}
\item{findclusters}{A list. First element of each list element contains matched name and
following elements contain aliases found. They do not have to be correct. Most will be not correct.
The list has to be manually checked.}
\item{final}{A character vector. Insert it into third column of result of \code{makeforest}.}
}
\details{
\item{normalizeauthors}{Discard bounces, e-mail domains, companies or locations in paratheses, middle names, titles, and some numbers}
\item{sortnames}{Isolate firest name, last name, and e-mail name.}
\item{emailfirst}{Write e-mail name first, then "|", then real name.}
\item{changenames}{Load dataset of already accepted aliases first (\code{take.memory.rda}).
      Replace all aliases by a single name.}
\item{findclusters}{Finds similar strings based on \code{base::agrep}. \code{changenames} has to be applied
    afterwards.}
\item{final}{Some final transformations of author names.}
}
\references{Based on Christian Bird, Alex Gourley, Prem Devanbu, Michael Gertz, and Anand Swaminathan.
  Mining Email Social Networks. 2006.
  In Proceedings of the 2006 international workshop on Mining software repositories, Shanghai, China.
  Pages 137-143. \url{http://macbeth.cs.ucdavis.edu/msr06.pdf}
}
}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
