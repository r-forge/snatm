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
\value{Prints \code{"Done"} when operation finished. Saves an \code{.rda} file with name \code{filename}.}
\details{Writing all e-mails in one file is necessary for \code{threads(tm.plugin.mail)} to assign correct
thread IDs to all the e-mails later in the process.}
\author{Angela Bohn \email{angela.bohn@gmail.com}}
\examples{
files <- c("2008-January.txt","2008-February.txt","2008-March.txt","2008-April.txt","2008-May.txt"
          ,"2008-June.txt","2008-July.txt","2008-August.txt","2008-September.txt","2008-October.txt","2008-November.txt","2008-December.txt"
          ,"2009-January.txt","2009-February.txt","2009-March.txt","2009-April.txt","2009-May.txt")
as.one.file(files,filename="allthreads",list="rdevel")
}