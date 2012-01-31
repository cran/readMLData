\name{checkConsistency}
\alias{checkConsistency}
\title{ Checks consistency of the data frame \code{dsList}.}
\description{
  Checks consistency of the parameters specified for each dataset in the \code{dsList}
  data frame created by \code{prepareDSList()}.
}
\usage{
checkConsistency(dsList, outputInd=FALSE)
}
\arguments{
\item{dsList}{ Data frame as created by \code{prepareDSList()}.}
\item{outputInd}{ Logical. Determines, whether the output should
  be a vector of indices of the data sets with conflicts.}
}
\value{
  Depending on \code{outputInd}, either a vector of indices of data sets with
  a conflict between the specified parameters or NULL invisibly.
}
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
  checkConsistency(dsList)
}
\author{ Petr Savicky }
\keyword{ data }

