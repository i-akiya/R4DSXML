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
define <- paste0("/Volumes/Transcend/develop/Rwork/VerifiyR4DSXML/TestData/",
                "ADaM/define2-0-0-example-adam.xml")
CT <- getCT(define)

}
