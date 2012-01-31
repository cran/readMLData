\name{getPath}
\alias{getPath}
\title{Determine the path to package example directories. }
\description{
  Appends the path to the directory of an installed package and
  a name of its subdirectory.
}
\usage{
getPath(dirName)
}
\arguments{
  \item{dirName}{ Character. Name of the example subdirectory of
   an installed package. This is currently \code{exampleDescription}
   or \code{exampleData}.}
}
\value{
  Character string, which is a full path to the required example
  directory in an installed package.
}
\seealso{
\code{\link{prepareDSList}}
}
\author{ Petr Savicky }
\keyword{ data }

