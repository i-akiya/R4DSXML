getCT <- function( filepath ) {
    doc = xmlTreeParse( filepath, useInternalNodes = T )
    namespaces <- namespaces(doc)
    
    cl <- getCodeListItem(doc, namespaces)
    enu <- getEnumeratedItem(doc, namespaces)
    mm <- merge(cl, enu, all=T, stringsAsFactors=FALSE)
    sortlist <- order(mm$OID, mm$OrderNumber, mm$Rank, mm$CodedValue)
    mm2 <- mm[sortlist,]
    return( mm2 )
}
