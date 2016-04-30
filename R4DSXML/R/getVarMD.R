getVarMD <- function( filepath ) {
    doc = xmlTreeParse( filepath, useInternalNodes = T )
    namespaces <- c( ns='http://www.cdisc.org/ns/odm/v1.3' )
    
    #ItemRef
    ItemGroupDef <- getNodeSet( doc, "//ns:ItemGroupDef", namespaces )
    DSName <- getDSName( ItemGroupDef )
    
    
    for ( i in DSName ){
        ItemRefNode <- getNodeSet( 
                          doc, 
                          paste( "//ns:ItemGroupDef[@Name ='", i, "']//ns:ItemRef", sep = "" ), 
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
        
        IR_KeySequence <- as.integer(
            getAttr(Nodeset=ItemRefNode, Attr="KeySequence")
        )
        
        IGD_Name <- i
        
        tmpdf <- data.frame(
            IGD_Name, 
            IR_ItemOID, 
            IR_OrderNumber, 
            IR_Mandatory, 
            IR_KeySequence,
            stringsAsFactors = FALSE
        )
        
        if ( is.na( match( "ItemRef", ls() ) ) ) {
            ItemRef <- tmpdf
        } else {
            ItemRef <- merge( ItemRef, tmpdf, all=T )
        }
        
    }
    # get ItemRef end 

    item_def <- getItemDef( doc )
    Variable.Metadata <- 
        merge( ItemRef, item_def, by.x = "IR_ItemOID", by.y = "ID_OID" )
    so <- order(
                Variable.Metadata$IGD_Name, 
                Variable.Metadata$IR_OrderNumber
                )
    
    Variable.Metadata <- Variable.Metadata[so, ]
    row.names(Variable.Metadata) <- NULL
    return( Variable.Metadata )
}
