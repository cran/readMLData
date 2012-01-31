\name{xml}
\alias{readDSListFromXML}
\alias{saveDSListAsXML}
\title{
  Handling XML files.
}
\description{
  Input and output of a data set description from and to a XML file. These functions
  are not inteded for direct use by the user for reading the data sets. The
  function \code{readDSListFromXML()} is called from \code{prepareDataDir()}.
  The function \code{saveDSListAsXML} is used for preparing the file
  \code{contents.xml} in the data set description directory.
}
\usage{
readDSListFromXML(filename)
saveDSListAsXML(dsList, filename)
}
\arguments{
\item{dsList}{ A data frame created by \code{prepareDataDirectory()}.}
\item{filename}{ The name of an XML file to be used. }
}
\value{
  \code{saveDSListAsXML()} returns the filename of the created file.
  \code{readDSListFromXML()}
  returns a data frame with the description of the data sets.
}
\author{ Petr Savicky }
\keyword{ data }

