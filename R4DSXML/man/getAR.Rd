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
define <- paste0("/path/to/data/directory/",
                "define_with_ARM.xml")
AR <- getAR(define)
}
