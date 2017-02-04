\name{getAR}
\alias{getAR}
\title{
Exract Analysis Results Metadata from Define-XML
}
\description{
The getAR function supports Analysis Results Metadata v1 for Define-XML v2.
}
\usage{
getAR(filepath)
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
AR <- getAR(define)

}
