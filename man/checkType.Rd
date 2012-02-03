\name{checkType}
\alias{checkType}
\title{Compares the type of columns stored in \code{dsList} and in a data set
itself.}
\description{
Compares types.
}
\usage{
checkType(dsList, id)
}
\arguments{
   \item{dsList}{ Data frame describing the data sets as produced by \code{prepareDSList()}.}
   \item{id}{ Numeric or character of length one. Index or the identification of a data set.}
}
\value{
Errors are printed. The value is \code{TRUE} or \code{FALSE}
according, whether the types are correct or not.
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

