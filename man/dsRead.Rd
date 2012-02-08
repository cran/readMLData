\name{dsRead}
\alias{dsRead}
\title{Loading machine learning data from a directory tree using
  a unified interface.}
\description{
  The function allows to read data sets included in the description
  in the data frame \code{dsList} into R environment using a unified interface.
}
\usage{
dsRead(dsList, id, responseName = NULL, originalNames=TRUE, deleteUnused=TRUE, keepContents=FALSE)
}
\arguments{
\item{dsList}{ Data frame as created by \code{prepareDSList()}.}
  \item{id}{ Name of the data set in \code{dsList$identification} or the index
    of the row in \code{dsList} corresponding to the data set.}
  \item{responseName}{ Character. The required name of the response column
    in the output data frame created from the data set.}
  \item{originalNames}{ If TRUE, the original names of columns are used, if they
    are present in the description XML file.}
  \item{deleteUnused}{ Logical. Controls, whether the columns containing case
    labels or other columns not suitable as attributes, are removed from the data.}
  \item{keepContents}{ Logical. If \code{TRUE}, then \code{deleteUnused}
    parameter is ignored and no columns are converted to factors.}
}
\details{
  The function uses \code{dsList$avaiable} to determine, whether the files for
  the required data set is present in the local directory \code{dsList$pathData}.
  If not, a corresponding error message is printed. See \code{prepareDSList()}
  and \code{getAvailable()}.
}
\value{
  A data frame containing the required data set, possibly transformed according
  to the setting of the parameters \code{responseName, originalNames, deleteUnused}.
  If an error occurred, the function outputs \code{NULL}.
}
\seealso{
\code{\link{readMLData}}, \code{\link{prepareDSList}}, \code{\link{getAvailable}}.
}
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
  dat <- dsRead(dsList, "glass")
  dim(dat)
}
\author{ Petr Savicky }
\keyword{ data }

