\name{getValMD}
\alias{getValMD}
\title{
Exract value level metadata from Define-XML
}
\description{
The getValMD function supports Define-XML version 2.0.
}
\usage{
getValMD(filepath)
}
\arguments{
  \item{filepath}{ 
  Character string specifying the file path of Define-XML. 
  }
}
\author{
Ippei Akiya
}
\examples{
## Not run: 
define <- paste0("/path/to/data/directory/",
                "define.xml")
ValMD <- getValMD(define)
}
