\name{checkAvailability}
\alias{checkAvailability}
\title{ Checks consistency of the data frame \code{dsList}.}
\description{
  Checks whether all the files of a specified data set are accesible
  in a local directory.
}
\usage{
checkAvailability(dsList, id)
}
\arguments{
  \item{dsList}{ Data frame as created by \code{prepareDSList()}.}
  \item{id}{ Character or numeric vector. A character vector should contain
  names matching the names \code{dsList$identification}. Numeric vector
  should consist of the indices of the rows in \code{dsList} corresponding
  to the data set.}
}
\value{
  Logical vector of the length \code{length(id)} specifying for
  each component of \code{id} the result of the check.
}
\seealso{
\code{\link{readMLData}}.
}
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
  checkAvailability(dsList, 1:nrow(dsList))
}
\author{ Petr Savicky }
\keyword{ data }

