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
define <- paste0("/Volumes/Transcend/develop/Rwork/VerifiyR4DSXML/TestData/",
                "SDTM/define2-0-0-example-sdtm(2013-11-09).xml")
ValMD <- getValMD(define)
}
