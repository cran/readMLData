\name{getAvailable}
\alias{getAvailable}
\title{ Checks consistency of the data frame \code{dsList}.}
\description{
  Checks whether all the files of a specified data set are accesible
  in a local directory.
}
\usage{
getAvailable(dsList, id=NULL, asLogical=FALSE)
}
\arguments{
  \item{dsList}{ Data frame as created by \code{prepareDSList()}.}
  \item{id}{ Character or numeric vector. A character vector should contain
  names matching the names \code{dsList$identification}. Numeric vector
  should consist of the indices of the rows in \code{dsList} corresponding
  to the data set. If \code{id=NULL}, then all data sets are checked.}
  \item{asLogical}{ Logical, whether the output should be a logical
  vector of the same length as \code{id} or a character vector containing
  the identification of the available data sets.}
}
\details{
  The test is not completely reliable, since it only verifies that
  the files with the required file name are accessible. If the
  files require some transformations after download and these
  are not performed, the data set is still reported as available.
  The test uses file names specified in \code{contents.xml} file.
  If these names are by mistake different from the files actually
  read in the reading scripts, then the test may also yield an
  incorrect result.
}
\value{
  Logical vector of the length \code{length(id)} specifying for
  each component of \code{id} the result of the check or a character
  vector containing the identification of the available data sets.
}
\seealso{
\code{\link{readMLData}}.
}
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
  getAvailable(dsList)
}
\author{ Petr Savicky }
\keyword{ data }

