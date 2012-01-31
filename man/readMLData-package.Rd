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
  The data are not part of the package.
  The description of the data sets consists of a directry, which
  contains an XML file \code{contents.xml}, which contains part
  of the information on all the data sets, and R scripts in 
  a subdirectory \code{scripts}, which allow to read individual
  data sets into R.

  An example of the description directory describing three UCI data sets
  is in \code{exampleDescription} subdirectory of the installed package.
  The data itself are in \code{exampleData} subdirectory.

  Use \code{help(package=readMLData)} or \code{library(help=readMLData)} to see
  the list of functions in the package.
}
\author{
Maintainer: Petr Savicky <savicky@cs.cas.cz>
}
\keyword{ package }

