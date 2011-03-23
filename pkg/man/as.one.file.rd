\name{as.one.file}
\alias{as.one.file}
\encoding{UTF-8}
\title{Write mailing list e-mails from several months in one file}
\description{Write mailing list e-mails from several months in one file.}
\usage{as.one.file(files,filename,list)}
\arguments{
\item{files}{A vector containing the names of files to be written in one file.}
\item{filename}{Filename of the file containing all e-mails from \code{files}.}
\item{list}{Which mailing list to use: \code{rhelp} or \code{rdevel}.}
}
\value{Saves an \code{.rda} file with name \code{filename}.}
\details{Writing all e-mails in one file is necessary for \code{threads(tm.plugin.mail)} to assign correct
thread IDs to all the e-mails later in the process.}
\author{Angela Bohn \email{angela.bohn@gmail.com}}
