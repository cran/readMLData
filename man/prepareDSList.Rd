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
  The list of fields, which should be included in \code{"contents.xml"},
  consists of the fields with either \code{usage=="obligatory"} or
  \code{usage=="optional"} in the table produced by \code{getFields()}.
  Fields with \code{usage=="additional"} and \code{usage=="computed"}
  are included automatically by the function \code{prepareDSList()}.

  The character "~" expands to your home directory.
}
\value{
  Data frame with columns \code{pathData}, \code{pathDescription},
  and some other as listed by \code{getFields()}. The output data frame
  can be used as \code{dsList} parametr of functions \code{dsSearch()},
  \code{dsRead()}, \code{checkConsistency()}, \code{checkType()}.
}
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
}
\author{ Petr Savicky }
\keyword{ data }

