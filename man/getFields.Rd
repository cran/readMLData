\name{getFields}
\alias{getFields}
\title{Prints the information on the fields in the data frame \code{dsList}
 describing the data sets.}
\description{
The data frame \code{dsList} contains names of the data sets, the names
of the directories, the files, which belong to each of the data sets,
and some other information. The function returns a table describing the
fields and their usage.
}
\usage{
getFields()
}
\value{
Table containing the names, types and usage of the fields expected
in \code{dsList}.
}
\author{ Petr Savicky }
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
  getFields()
}
\keyword{ data }

