getCT <- function( filepath ) {
    cl <- getCodeListItem(filepath)
    enu <- getEnumeratedItem(filepath)
    mm <- merge(cl, enu, all=T, stringsAsFactors=FALSE)
    sortlist <- order(mm$OID, mm$OrderNumber, mm$Rank, mm$CodedValue)
    mm2 <- mm[sortlist,]
    return( mm2 )
}
