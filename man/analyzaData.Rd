\name{analyzeData}
\alias{analyzeData}
\title{Determine the type of values in each column of a data frame.}
\description{
For each column, its class and the number of different values is determined.
For numeric columns, also the minimum and maximum is computed.
}
\usage{
analyzeData(dat)
}
\arguments{
   \item{dat}{ A data frame.}
}
\value{
  A data frame with columns \code{"class", "num.unique", "min", "max"}, which
  correspond to properties of columns of \code{dat}. The rows in the output
  data frame correspond to the columns of \code{dat}.
}
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
  dat <- dsRead(dsList, "glass")
  analyzeData(dat)
}
\author{ Petr Savicky }
\keyword{ data }

