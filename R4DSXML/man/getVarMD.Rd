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
define <- paste0("/path/to/data/directory/",
                "define.xml")
VarMD <- getVarMD(define)
}
