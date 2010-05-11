\name{threads}
\alias{threads}
\encoding{UTF-8}
\title{Threads}
\description{A fixed version of the threads function of the tm.plugin.mail package.}
\usage{threads(x)}
\arguments{
\item{x}{A corpus consisting of e-mails.}
}
\value{A list with the two named components \texttt{ThreadID} and \texttt{ThreadDepth},
listing a thread and the level of replies for each mail in the corpus \texttt{x}.}
\author{Ingo Feinerer and Angela Bohn \email{angela.bohn at gmail.com}}