\name{read.dataset.xml}
\alias{read.dataset.xml}
\title{
Read a Dataset-XML file as a data frame
}
\description{
This function returns study data as R dataframe converted from Dataset-XML.
}
\usage{
read.dataset.xml(dataset_xml, define_xml)
}
\arguments{
  \item{dataset_xml}{
    Character string specifying the file path of Dataset-XML.
}
  \item{define_xml}{
    Character string specifying the file path of Define-XML.
}
}
\details{
Variable types in returned dataframe are converted based on following rule. \cr
\tabular{lll}{
ODM data type \tab  \tab R data type \cr
string \tab  \tab character \cr
integer \tab  \tab integer \cr
float \tab  \tab double\cr
date \tab  \tab character\cr
time \tab  \tab character\cr
datetime \tab  \tab character\cr
}
}

\value{
This function returns a dataframe.
}
\author{
Ippei Akiya
}
\examples{
## Not run: 
define <- paste0("/path/to/data/directory/",
                "define.xml")
sds <- paste0("/path/to/data/directory/",
                "dm.xml")

DM <- read.dataset.xml(dataset_xml=sds, define_xml=define)
}
