\name{readMLData-package}
\alias{readMLData-package}
\alias{readMLData}
\docType{package}
\title{
Reading data from different sources in their original format.
}
\description{
  The package contains functions, which allow to maintain and use
  a structure describing a collection of machine learning datasets
  and read them into R environment using a unified interface, see
  function \code{prepareDSList()} and \code{dsRead()}.
}
\details{
  The data are not part of the package. The package requires to
  receive a path to a local copy of the data and their description.
  The description of the data sets consists of a directory, which
  contains an XML file \code{contents.xml} and subdirectory "scripts",
  which contains an R script for each data set, which reads the
  data set into R. File \code{contents.xml} contains information
  on all the data sets. In particular it contains their names for
  local identification, their public names, and the names of files
  representing the data set. The name of the script for reading
  a data set is derived from its identification name. The complete
  list of the fields in \code{contents.xml} may be obtained using
  \code{getFields()}.

  For the simplest use of the package for reading the data sets, the
  functions \code{prepareDSList()} and \code{dsRead()} are sufficient.
  The remaining functions are useful for including further data sets to
  the description. Use \code{help(package=readMLData)} or
  \code{library(help=readMLData)} to see the list of functions.

  The list of fields, which should be included in \code{"contents.xml"},
  consists of the fields with either \code{usage=="obligatory"} or
  \code{usage=="optional"} in the table produced by \code{getFields()}.
  Fields with \code{usage=="additional"} and \code{usage=="computed"}
  are included automatically by the function \code{prepareDSList()}.

  An example of the description directory describing three UCI data sets
  is in \code{exampleDescription} subdirectory of the installed package.
  The data themselves are in \code{exampleData} subdirectory. See
  \url{http://www.cs.cas.cz/~savicky/readMLData/} for description
  files of further data sets from UCI Machine Learning Repository.
}
\references{
UCI Machine Learning Repository,
\url{http://archive.ics.uci.edu/ml/}.

Additional resources for the CRAN package readMLData,
\url{http://www.cs.cas.cz/~savicky/readMLData/}.
}
\author{
Petr Savicky
}
\keyword{ package }

