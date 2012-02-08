\name{checkType}
\alias{checkType}
\title{Compares the type of columns stored in \code{dsList} and in a data set
itself.}
\description{
Compares types.
}
\usage{
checkType(dsList, id, dat=NULL)
}
\arguments{
   \item{dsList}{ Data frame describing the data sets as produced by \code{prepareDSList()}.}
   \item{id}{ Numeric or character of length one. Index or the identification of a data set.}
   \item{dat}{ An optional data frame as read by \code{dsRead(dsList, id, keepContents=TRUE)}.}
}
\value{
The name of the tested data set and the result of the test is printed.
If errors are found, a more detailed message is printed. The output value
is \code{TRUE} or \code{FALSE} invisibly according, whether the types are
correct or not.
}
\seealso{
\code{\link{readMLData}}.
}
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
  checkType(dsList, 1)
}
\author{ Petr Savicky }
\keyword{ data }

