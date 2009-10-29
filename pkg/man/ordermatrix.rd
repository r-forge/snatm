\name{ordermatrix}
\alias{ordermatrix}
\encoding{UTF-8}
\title{Order the rows of a matrix according to the values in a certain column}
\description{Order the rows of a matrix according to the values in a certain column.}
\usage{ordermatrix(input,by)}
\arguments{
\item{input}{An input matrix.}
\item{by}{A numeric value specifying the column to be ordered. All rows of \code{input} will be ordered according to the new order of
\code{by}.}
}
\value{A matrix whose rows are ordered according to the new order of the \code{by}th column.}
\author{Angela Bohn \email{angela.bohn at gmail.com}}
\examples{
input <- matrix(c(4,2,1,3,5,"right","is","This","the","order."),ncol=2)
ordermatrix(input,by=1)
}