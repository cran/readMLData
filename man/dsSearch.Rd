\name{dsSearch}
\alias{dsSearch}
\title{Search a dataset by string matching against the names stored
in \code{dsList}.}
\description{
  The function allows string matching against some of the fields
  \code{"identification", "fullName", "dirName", "files"} of the
  structure describing the data sets.
}
\usage{
dsSearch(dsList, id, searchField=c("identification", "fullName", "dirName", "files"),
            searchType=c("exact", "prefix", "suffix", "anywhere"), caseSensitive=FALSE)
}
\arguments{
\item{dsList}{ Data frame as created by \code{prepareDSList()}.}
\item{id}{ Character of length one or numeric of length at most \code{nrow(dsList)}.
  If character, then it is used as a search string to be matched against
  the names of datasets. If numeric, it is used as indices of data sets
  in \code{dsList}.}
\item{searchField}{ Character. Name of a column in \code{dsList} to be searched.}
\item{searchType}{ Character. Type of search.}
\item{caseSensitive}{ Logical. Whether the search should be case sensitive.}
}
\details{
  The parameter \code{searchField} determines, which column of \code{dsList}
  is searched, parameters \code{searchType} and \code{caseSensitive}
  influence the type of search. These three parameters are ignored, if
  \code{id} is numeric.

  Regular expressions are not used. Matching with \code{searchType="exact"}
  is done with \code{==}, \code{searchType="prefix"} and \code{searchType="suffix"}
  are implemented using \code{substr()}, \code{searchType="anywhere"} is
  implemented using \code{grep(, fixed=TRUE)}.
}
\value{
  Data frame containing the indices and identification of the matching data
  sets and the value of the search field, if applicable.
}
\seealso{
\code{\link{readMLData}}.
}
\examples{
  pathData <- getPath("exampleData")
  pathDescription <- getPath("exampleDescription")
  dsList <- prepareDSList(pathData, pathDescription)
  dsSearch(dsList, "ident", searchField="fullName", searchType="anywhere")
}
\author{ Petr Savicky }
\keyword{ data }

