\name{getStudyMD}
\alias{getStudyMD}
\title{
Exract Study Metadata from Define-XML
}
\description{
The getStudyMD function returns Study Metadata in define.xml as the named vector.

}
\usage{
getStudyMD(filepath)
}
\arguments{
  \item{filepath}{
Character string specifying the file path of define.xml.
}
}
\value{
The returned named vetor includes the data of Study Name named, Study Description, Protocol Name, Define Version, Standard Name, and Standard Version. 
The specified name for these data is "StudyName", "StudyDescription", "ProtocolName", "DefineVersion", "StandardName", and "StandardVersion" respectively.
}
\author{
Ippei Akiya
}
\examples{
## Not run: 
define <- paste0("/path/to/data/directory/",
                "define.xml")
StudyMetaData <- getStudyMD(define)
## "CDISC01" is printed on console output
print(StudyMetaData["StudyName"])

}
