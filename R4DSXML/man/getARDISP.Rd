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
define <- paste0("/path/to/data/directory/",
                "define_with_ARM.xml")
AR_Displays <- getARDISP(define)

}
