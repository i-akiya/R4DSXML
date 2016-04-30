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
define <- system.file("extdata", "SDTM",
                      "define2-0-0-example-sdtm(2013-11-09).xml", 
                       package="R4DSXML"
                       ) 
ValMD <- getValMD(define)
}
