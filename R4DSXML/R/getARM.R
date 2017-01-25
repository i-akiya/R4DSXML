getARM <- function(filepath) {
    doc = xmlTreeParse(filepath, useInternalNodes = T)
    namespaces <- c(ns = 'http://www.cdisc.org/ns/odm/v1.3', 
                    arm='http://www.cdisc.org/ns/arm/v1.0')
    
    #AnalysisResultDisplays Metadata
    AnalysisResultDisplays <<- getNodeSet(doc, "//arm:AnalysisResultDisplays//arm:ResultDisplay", namespaces)
    aryAttr <- c( "OID", "Name" )
    
    ARD_OID <- getAttr(Nodeset = AnalysisResultDisplays, Attr = "OID")
    ARD_Name <- getAttr(Nodeset = AnalysisResultDisplays, Attr = "Name")
    
    # IGD <-
    #   getNodeSet(doc, "//ns:ItemGroupDef//ns:Description", namespaces)
    # IGD_Description <-
    #   sapply(IGD, function(el)
    #     xmlValue(el, 'Description/TranslatedText[@xml:lang = "en"]'))
    # IGD_title <-
    #   sapply(IGD, function(el)
    #     xmlValue(el, "def:leaf/def:title"))
    # 
      df <- data.frame(
        ARD_OID,
        ARD_Name
    #   IGD_Repeating,
    #   IGD_IsReferenceData,
    #   IGD_SASDatasetName,
    #   IGD_Purpose,
    #   IGD_Structure,
    #   IGD_Class,
    #   IGD_ArchiveLocationID,
    #   IGD_Description,
    #   IGD_title,
    #   stringsAsFactors = FALSE
      )
    row.names(df) <- NULL
    return(df)
  }
