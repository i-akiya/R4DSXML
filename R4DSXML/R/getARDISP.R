getARDISP <- function(filepath) {
  doc = xmlTreeParse(filepath, useInternalNodes = T)
  namespaces <- namespaces(doc)
  
  #AnalysisResultDisplays Metadata
  AnalysisResultDisplays <-
    getNodeSet(doc,
               "//arm:AnalysisResultDisplays/arm:ResultDisplay",
               namespaces)
  aryAttr <- c("OID", "Name")
  
  ARD_OID <-
    getAttr(Nodeset = AnalysisResultDisplays, Attr = "OID")
  ARD_Name <-
    getAttr(Nodeset = AnalysisResultDisplays, Attr = "Name")
  
  ARD_DES_ND <-
     getNodeSet(doc,
                "//arm:AnalysisResultDisplays/arm:ResultDisplay/ns:Description",
                namespaces)
  
  ARD_Description <-
    sapply(ARD_DES_ND, function(el)
      xmlValue(el, 'TranslatedText[@xml:lang = "en"]'))
  ARD_Description <- str_trim(ARD_Description, side="right")
  
  df <- data.frame(ARD_OID,
                   ARD_Name,
                   ARD_Description, stringsAsFactors = FALSE)
  row.names(df) <- NULL
  return(df)
}
