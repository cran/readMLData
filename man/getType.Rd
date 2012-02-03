\name{getType}
\alias{getType}
\title{Determines the type vector for an input data set.}
\description{
The type information is derived from the contents of individual columns
of an input data frame.
}
\usage{
getType(dat)
}
\arguments{
   \item{dat}{ A data frame.}
}
\value{
  A character vector of length \code{ncol(dat)} containing "n" for numerical
  columns, the number of different values for character or factor columns,
  and "o" otherwise.
}
\seealso{
\code{\link{readMLData}}.
}
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
  dat <- dsRead(dsList, "annealing")
  getType(dat)
}
\author{ Petr Savicky }
\keyword{ data }

