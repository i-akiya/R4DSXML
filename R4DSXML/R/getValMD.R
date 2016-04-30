getValMD <- function( filepath ) {
    doc = xmlTreeParse( filepath, useInternalNodes = T )
    namespaces <- c( ns='http://www.cdisc.org/ns/odm/v1.3', 
                     def='http://www.cdisc.org/ns/def/v2.0' )
    
    #ItemRef
    ValueListDefNode <- getNodeSet( doc, "//def:ValueListDef", namespaces )
    ValueListDef <- getAttr( Nodeset=ValueListDefNode,  Attr="OID")
    
    
    for ( i in ValueListDef ){
      ItemRefNode <- getNodeSet( 
        doc, 
        paste( "//def:ValueListDef[@OID ='", i, "']//ns:ItemRef", sep = "" ), 
        namespaces
      )
 
      IR_ItemOID <- getAttr( 
        Nodeset = ItemRefNode, 
        Attr = "ItemOID"
      )
      
      IR_OrderNumber <- as.integer(
        getAttr(
          Nodeset=ItemRefNode, 
          Attr="OrderNumber"
        )
      )
      
      IR_Mandatory <- getAttr( Nodeset=ItemRefNode, Attr="Mandatory" )
      
      #  IR_KeySequence <- as.integer(
      #    getAttr(Nodeset=ItemRefNode, Attr="KeySequence")
      #  )
      
      ValueListOID <- i
      
      tmpdf <- data.frame(
        ValueListOID, 
        IR_ItemOID, 
        IR_OrderNumber, 
        IR_Mandatory, 
        stringsAsFactors = FALSE
      )
      
      if ( is.na( match( "ItemRef", ls() ) ) ) {
        ItemRef <- tmpdf
      } else {
        ItemRef <- merge( ItemRef, tmpdf, all=T )
      }
      
    }
    ### get ItemRef end ####################################################

    item_def <- getItemDef( doc )
    Value.Metadata <- 
        merge( ItemRef, item_def, by.x = "IR_ItemOID", by.y = "ID_OID" )
    so <- order(
                Value.Metadata$ValueListOID, 
                Value.Metadata$IR_OrderNumber
                )
    
    Value.Metadata <- Value.Metadata[so, ]
    row.names(Value.Metadata) <- NULL
    return( Value.Metadata )
}
