# R4DSXML

R4DSXML is R package for import CDISC Dataset-XML and Define-XML as R data frame.

## Features
* Support CDISC Dataset-XML 1.0 and Define-XML 2.0.
* Covert from Dataset-XML to R dataframe.
* Extract following metadata from Define-XML as R dataframe.
  * Dataset level metadata
  * Variable level metadata
  * Value level metadata
  * Controlled Terms

## Installation
* From Github  
Run following command in R console.  
library(devtools)  
install_github("DataDrivenInc/R4DSXML/R4DSXML")

## Dependencies
* [R](http://cran.r-project.org/) (>= 3.0.3)
* [R package XML](http://cran.r-project.org/web/packages/XML/index.html) (>= 3.98-1.1)

## License
[The GNU Lesser General Public License, version 3.0 (LGPL-3.0)](http://opensource.org/licenses/lgpl-3.0.html)

## Author
 [Ippei Akiya](http://github.com/i-akiya)

## References
* [Dataset-XML](https://www.cdisc.org/standards/foundational/dataset-xml)
* [Define-XML](https://www.cdisc.org/standards/foundational/define-xml)
