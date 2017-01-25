\name{getDLMD}
\alias{getDLMD}
\title{
Exract dataset level metadata from Define-XML
}
\description{
The getDLMD function supports define.xml version 2.0.
}
\usage{
getDLMD(filepath)
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
define <- system.file("extdata", "SDTM",
                      "define2-0-0-example-sdtm(2013-11-09).xml", 
                       package="R4DSXML"
                       ) 
DLMD <- getDLMD(define)

}
