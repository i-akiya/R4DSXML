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
define <- paste0("/path/to/data/directory/",
                "define.xml")
DLMD <- getDLMD(define)

}
