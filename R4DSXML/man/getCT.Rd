\name{getCT}
\alias{getCT}
\title{
Exract Controlled Terminology from Define-XML
}
\description{
The getCT function supports define.xml version 2.0. This function returns controlled terms in define.xml as R dataframe.

}
\usage{
getCT(filepath)
}
\arguments{
  \item{filepath}{
Character string specifying the file path of define.xml.
}
}
\author{
Ippei Akiya
}
\examples{
## Not run: 
define <- paste0("/path/to/data/directory/",
                "define.xml")
CT <- getCT(define)

}
