getAR <- function( filepath ) {
    doc = xmlTreeParse( filepath, useInternalNodes = T )
    namespaces <- c(ns = 'http://www.cdisc.org/ns/odm/v1.3', 
                    def='http://www.cdisc.org/ns/def/v2.0',
                    arm='http://www.cdisc.org/ns/arm/v1.0')
    
    AnalysisResultNode <- getNodeSet(doc, "//arm:AnalysisResultDisplays/arm:ResultDisplay/arm:AnalysisResult", namespaces)
    AR_OID <- getAttr( Nodeset = AnalysisResultNode,  Attr="OID")
    AR_ParameterOID <- getAttr( Nodeset = AnalysisResultNode,  Attr="ParameterOID")
    AR_AnalysisReason <- getAttr( Nodeset = AnalysisResultNode,  Attr="AnalysisReason")
    AR_AnalysisPurpose <- getAttr( Nodeset = AnalysisResultNode,  Attr="AnalysisPurpose")
    
    AR_DES_ND <- getNodeSet(doc, "//arm:AnalysisResultDisplays/arm:ResultDisplay/arm:AnalysisResult", namespaces)
    AR_Description <- sapply(AR_DES_ND, function(el) xmlValue(el, 'Description/TranslatedText[@xml:lang = "en"]'))
    
    AR_Doc <- getNodeSet(doc, "//arm:AnalysisResultDisplays/arm:ResultDisplay/arm:AnalysisResult/arm:Documentation", namespaces)
    AR_Doc_Description <- sapply(AR_Doc, function(el) xmlValue(el, 'Description/TranslatedText[@xml:lang = "en"]'))
    
    AR_DocRef_ND <- getNodeSet(doc, "//arm:AnalysisResultDisplays/arm:ResultDisplay/arm:AnalysisResult/arm:Documentation/def:DocumentRef", namespaces)
    AR_DocRef <- getAttr( Nodeset = AR_DocRef_ND,  Attr="leafID")
    
#    AR_Prg_ND <- getNodeSet(doc, "//arm:AnalysisResultDisplays/arm:ResultDisplay/arm:AnalysisResult/arm:ProgrammingCode", namespaces)
#    AR_Prg_Context <- getAttr( Nodeset = AR_Prg_ND,  Attr="Context")
#    AR_Prg_Code <- sapply(AR_Prg_ND, function(el) xmlValue(el, 'arm:Code'))
    
    
    df <- data.frame(
      AR_OID,
      AR_ParameterOID,
      AR_AnalysisReason,
      AR_AnalysisPurpose,
      AR_Description,
      AR_Doc_Description,
      AR_DocRef
#      AR_Prg_Context,
#      AR_Prg_Code
      )
    
    return( df )
}
