getItemDef <- function( Nodeset ){

    namespaces <- namespaces()
    ItemDefNode <- getNodeSet(Nodeset, "//ns:ItemDef", namespaces)
    
    ID_OID <- getAttr(Nodeset=ItemDefNode, Attr="OID")
    ID_Name <- getAttr(Nodeset=ItemDefNode, Attr="Name")
    
    ID_DataType <- getAttr(Nodeset=ItemDefNode, Attr="DataType")
    ID_Length <- getAttr(Nodeset=ItemDefNode, Attr="Length")
    ID_SignificantDigits <- getAttr(Nodeset=ItemDefNode, Attr="SignificantDigits")
    ID_SASFieldName <- getAttr(Nodeset=ItemDefNode, Attr="SASFieldName")
    ID_SASFormatName <- getAttr(Nodeset=ItemDefNode, Attr="SASFormatName")
    
    ItemDefNode2 <- getNodeSet(Nodeset, "//ns:ItemDef/ns:Description", namespaces)
    ID_Label <- getVal(ItemDefNode2, 'TranslatedText[@xml:lang = "en"]')
    
    ID_CodeListOID <- getCodeListRef(ItemDefNode)

    #ItemDefNode4 <- getNodeSet(Nodeset, "//ns:ItemDef/def:Origin", namespaces)
    originList <- getOrigin(ItemDefNode)
    ID_OriginType <- originList[[1]]
    ID_OriginDescription <- originList[[2]]
      
    #getVal(ItemDefNode4, 'TranslatedText[@xml:lang = "en"]')
    
    
    df <- data.frame(
        ID_OID,
        ID_Name, 
        ID_Length, 
        ID_SignificantDigits,
        ID_DataType, 
        ID_Label, 
        ID_SASFieldName,
        ID_SASFormatName,
        ID_CodeListOID,
        ID_OriginType,
        ID_OriginDescription,
        stringsAsFactors = FALSE
        )
      
}
    