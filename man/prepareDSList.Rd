\name{prepareDSList}
\alias{prepareDSList}
\title{Prepares a data frame \code{dsList}, which describes the data contained
  in a local data description directory. }
\description{
  The data frame \code{dsList} is needed to read the data contained in
  a directory tree below \code{dsList$pathData} using \code{dsRead()}.
  The directory \code{pathDescription} is expected to contain the
  file \code{contents.xml} and subdirectory \code{scripts} with R scripts
  for reading the data sets.
}
\usage{
prepareDSList(pathData, pathDescription)
}
\arguments{
  \item{pathData}{ Character. A path to the required data directory.}
  \item{pathDescription}{ Character. A path to a directory containing
    description of the required data, in particular the file \code{"contents.xml"}.}
}
\details{
  The character "~" expands to your home directory.

  The directory \code{pathData} need not contain all the data sets
  included in \code{pathDescription/contents.xml}. The function
  \code{getAvailable()} is called and its output is stored
  in column \code{availability} of the output data frame, which is
  \code{logical} and specifies for each data set, whether it is or
  is not present.

  See \url{http://www.cs.cas.cz/~savicky/readMLData/} for description
  files of some of the data sets from UCI Machine Learning Repository.
  See the help page \code{\link{readMLData}} for more information
  on the structure of the description files.
}
\value{
  Data frame with columns \code{pathData}, \code{pathDescription},
  and other as listed by \code{getFields()}. The output data frame
  can be used as \code{dsList} parametr of functions \code{dsSearch()},
  \code{dsRead()}, \code{checkConsistency()}, \code{checkType()}.
}
\seealso{
\code{\link{readMLData}}, \code{\link{getAvailable}}, \code{\link{checkConsistency}}.
}
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
}
\author{ Petr Savicky }
\keyword{ data }

