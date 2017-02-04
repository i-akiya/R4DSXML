\name{getARDISP}
\alias{getARDISP}
\title{
Exract Analysis Result Displays Metadata from Define-XML
}
\description{
The getARDISP function supports Analysis Results Metadata v1 for Define-XML v2.
}
\usage{
getARDISP(filepath)
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
define <- system.file("extdata", "ARM",
                      "define2-0-0-example-adam-results.xml", 
                      package="R4DSXML"
                      ) 
AR_Displays <- getARDISP(define)

}
