getAR <- function( filepath ) {
    doc = xmlTreeParse( filepath, useInternalNodes = T )
    namespaces <- namespaces()
    
    AnalysisResultNode <- getNodeSet(doc, "//arm:AnalysisResultDisplays/arm:ResultDisplay/arm:AnalysisResult", namespaces)
    AR_OID <- getAttr( Nodeset = AnalysisResultNode,  Attr="OID")
    #AR_ParameterOID <- getAttr( Nodeset = AnalysisResultNode,  Attr="ParameterOID")
    AR_AnalysisReason <- getAttr( Nodeset = AnalysisResultNode,  Attr="AnalysisReason")
    AR_AnalysisPurpose <- getAttr( Nodeset = AnalysisResultNode,  Attr="AnalysisPurpose")
    
    AR_DES_ND <- getNodeSet(doc, "//arm:AnalysisResultDisplays/arm:ResultDisplay/arm:AnalysisResult/ns:Description", namespaces)
    AR_Description <- sapply(AR_DES_ND, function(el) xmlValue(el, 'TranslatedText[@xml:lang = "en"]'))
    AR_Description <- str_trim(AR_Description, side="right")
    
    AR_Doc <- getNodeSet(doc, "//arm:AnalysisResultDisplays/arm:ResultDisplay/arm:AnalysisResult/arm:Documentation/ns:Description", namespaces)
    AR_Doc_Description <- sapply(AR_Doc, function(el) xmlValue(el, 'TranslatedText[@xml:lang = "en"]'))
    AR_Doc_Description <- str_trim(AR_Doc_Description, side="right")
    
    AR_DocRef_ND <- getNodeSet( doc, "//arm:AnalysisResultDisplays/arm:ResultDisplay/arm:AnalysisResult/arm:Documentation/def:DocumentRef", namespaces)
    AR_DocRef <- getAttr( Nodeset = AR_DocRef_ND,  Attr="leafID")
    
    AR_Datasets <- getARDS( filepath, AR_OID )
    
    df <- data.frame(
      AR_OID,
      #AR_ParameterOID,
      AR_AnalysisReason,
      AR_AnalysisPurpose,
      AR_Description,
      AR_Doc_Description,
      AR_DocRef,
      stringsAsFactors = FALSE
      )
    
    
    df <- merge( df, AR_Datasets, by="AR_OID", all=T)
    return( df )
}
