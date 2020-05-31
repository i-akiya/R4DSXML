\name{getVarMD}
\alias{getVarMD}
\title{
Exract variable level metadata from Define-XML
}
\description{
The getVarMD function supports Define-XML version 2.0.
}
\usage{
getVarMD(filepath)
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
                "ADaM/define2-0-0-example-adam.xml")
VarMD <- getVarMD(define)
}
