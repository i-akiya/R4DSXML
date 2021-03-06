getItemDef <- function(doc) {
  namespaces <- namespaces(doc)
  ItemDefNode <- getNodeSet(doc, "//ns:ItemDef", namespaces)
  defVersion <-
    tail(str_split(namespaces[["def"]], "/", n = Inf, simplify = FALSE)[[1]], n = 1)
  
  ID_OID <- getAttr(Nodeset = ItemDefNode, Attr = "OID")
  ID_Name <- getAttr(Nodeset = ItemDefNode, Attr = "Name")
  
  ID_DataType <- getAttr(Nodeset = ItemDefNode, Attr = "DataType")
  ID_Length <- as.integer(getAttr(Nodeset = ItemDefNode, Attr = "Length"))
  ID_SignificantDigits <-
    as.integer(getAttr(Nodeset = ItemDefNode, Attr = "SignificantDigits"))
  ID_SASFieldName <-
    getAttr(Nodeset = ItemDefNode, Attr = "SASFieldName")
  if (defVersion == "v2.1") {
    ID_DisplayFormat <-
      getAttr(Nodeset = ItemDefNode, Attr = "def:DisplayFormat")
  } else {
    ID_SASFormatName <-
      getAttr(Nodeset = ItemDefNode, Attr = "ns:SASFormatName")
  }
  
  ID_Label <- getDescription(ItemDefNode,  namespaces)
  
  ID_CodeListOID <- getCodeListRef(ItemDefNode, namespaces)

  originList <- getOrigin(ItemDefNode,  namespaces)
  ID_OriginType <- originList[[1]]
  ID_OriginDescription <- originList[[2]]
  
  ID_ValueListOID <- getValueListRef(ItemDefNode, namespaces)
  
  
  if (defVersion == "v2.1") {
    df <- data.frame(
      ID_OID,
      ID_Name,
      ID_Length,
      ID_SignificantDigits,
      ID_DataType,
      ID_Label,
      ID_SASFieldName,
      ID_DisplayFormat,
      ID_CodeListOID,
      ID_OriginType,
      ID_OriginDescription,
      ID_ValueListOID,
      stringsAsFactors = FALSE
    )
  } else {
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
      ID_ValueListOID,
      stringsAsFactors = FALSE
    )
  }
}
