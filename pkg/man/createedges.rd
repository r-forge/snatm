\name{createedges}
\alias{createedges}
\title{Create edgelist from forest (result of \code{makeforest})}
\description{Create edgelist from forest (result of \code{makeforest}).}
\usage{createedges(forest,subjectfilter,contentfilter,lv)}
\arguments{
\item{forest}{A matrix with five colums. Result of \code{makeforest}}
\item{subjectfilter}{A regular expression. An edgelist of all people who used \code{subjectfilter} in the subject will be created.}
\item{contentfilter}{A regular expression. An edgelist of all people who used \code{contentfilter} in the content will be created.}
\item{lv}{What line values should represent.
\code{nom} ("number of mails") if the line value should represent the number of mails (equals 1).
\code{d} if line value should represent the inverse of the distance of the two authors in the time tree.}
}
\value{A matrix with a maximum of six columns.
First column contains author of the mail.
Second column contains an author of a mail written before.
Third column contains the line value.
Forth column contains the thread ID of the thread in which the e-mail was sent.
Fifth column contains the subject.
Sixth column contains the content.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
\examples{
forest <- rbind(
c(1,1,"PersonA","[R] Question on rhelp","Hello, I have a question...")
,c(2,1,"PersonB","[R] Question on rhelp","This is the answer")
,c(3,2,"PersonC","[R] Question on rdevel","Dear all,...")
,c(4,2,"PersonD","[R] Question on rdevel","Answer")
)
colnames(forest) <- c("EMailID","ThreadID","Author","Subject","Content")
createedges(forest,subjectfilter="rhelp")
createedges(forest,contentfilter="[A|a]nswer")
}