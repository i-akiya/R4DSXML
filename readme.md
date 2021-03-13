# R4DSXML

R4DSXML is R package for import CDISC Dataset-XML and Define-XML as R data frame.

## Features
* Support CDISC Dataset-XML 1.0, Define-XML 2.0,  :new: Define-XML 2.1 and Analysis Results Metadata 1.0.
* Covert from Dataset-XML to R dataframe.
* Extract following metadata from Define-XML as R dataframe.
  * Dataset level metadata
  * Variable level metadata
  * Value level metadata
  * Controlled Terms
  * Analysis Results Metadata


## Installation
* From Github with devtools <br>
Run following command in R console.  
```R
library(devtools)  
install_github("i-akiya/R4DSXML/R4DSXML")
```
* From package archive file <br>
It is able to obtain source package from [Release Page](https://github.com/i-akiya/R4DSXML/releases).

## Dependencies
* [R](https://cran.r-project.org/) (>= 3.6.0)
* [R package XML](https://cran.r-project.org/web/packages/XML/index.html)
* [R package stringr](https://cran.r-project.org/web/packages/stringr/index.html)

## License
[The GNU Lesser General Public License, version 3.0 (LGPL-3.0)](http://opensource.org/licenses/lgpl-3.0.html)

## Author
 [Ippei Akiya](https://github.com/i-akiya)

## Contributor
 [Masafumi Okada](https://github.com/mokjpn), He expanded the feature to support getting def:ValueListRef value as metadata.
  [Jean-Michel Bodart](https://github.com/jmbo1190), He found a bug that is not able to get whole variable metadata and value level metadata from define.xml in case of description element is not found.


## References
* [Dataset-XML](https://www.cdisc.org/standards/foundational/dataset-xml)
* [Define-XML](https://www.cdisc.org/standards/foundational/define-xml)
* [Analysis Results Metadaa](https://www.cdisc.org/standards/foundational/analysis-data-model-adam/analysis-results-metadata-v10)
