\name{dsSort}
\alias{dsSort}
\title{Sort the rows of a data frame.}
\description{
  Sort the rows of a data frame lexicographically. This
  allows to compare two data sets as sets of cases disregarding
  their order.
}
\usage{
dsSort(dat)
}
\arguments{
\item{dat}{ a dataframe.}
}
\details{
  The function calls \code{order()} with the columns of \code{dat} as the
  sorting criteria.
}
\value{
   Data frame, whose rows are reordered by the sorting.
}
\author{ Petr Savicky }
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
  dat <- dsRead(dsList, "glass")
  sorted <- dsSort(dat)
}
\keyword{ data }

