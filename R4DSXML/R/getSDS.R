getSDS <- function(xml_doc){
      namespaces <- c(ns='http://www.cdisc.org/ns/odm/v1.3', 
                      data='http://www.cdisc.org/ns/Dataset-XML/v1.0')

      ItemGroupData <- getNodeSet(xml_doc, "//ns:ItemGroupData", namespaces)
      ItemGroupDataSeq <- getItemGroupDataSeq( ItemGroupData )
      ItemGroupOID <- unique( getAttr(ItemGroupData, "ItemGroupOID") )

      for (i in ItemGroupDataSeq) {
          xpath_stat <- paste('//ns:ItemGroupData[@data:ItemGroupDataSeq="', 
                              i, 
                              '"]/ns:ItemData', 
                              sep=""
                              )
          ItemGroupDef <- getNodeSet(xml_doc, xpath_stat, namespaces)
          row_id <- c(as.integer(i))
          ItemOID <- sapply(ItemGroupDef, 
                            function(el) xmlGetAttr(el, "ItemOID", default = "")
                            )
          ID_Val <- sapply(ItemGroupDef, 
                          function(el) xmlGetAttr(el, "Value", default = "")
                          )
  
          if (i == ItemGroupDataSeq[1]){
              temp_df_ <- data.frame(row_id, 
                                    ItemOID, 
                                    ID_Val, 
                                    stringsAsFactors = FALSE,
                                    row.names = NULL)  
          }else{
              cur_df_ <- data.frame(row_id, 
                                   ItemOID, 
                                   ID_Val, 
                                   stringsAsFactors = FALSE,
                                   row.names = NULL)  
              temp_df_ <- merge(temp_df_, cur_df_, all = row_id)
          } 
      }

    df_ <- reshape(temp_df_, 
                  timevar = "ItemOID", 
                  idvar = "row_id", 
                  direction = "wide"
          )

    # rename to variable name
    i_oids <- names(df_)
    for(i in c(1:length(i_oids)) ){
        splt <- unlist(strsplit(i_oids[[i]], '[.]'))
        varnm <- splt[length(splt)]
        if( i==1 ){
            var_names <- c(varnm)
        }
        else{
            var_names <- c(var_names, varnm)
        }
    }
    
    names(df_) <- var_names
    df_ <- df_[setdiff(colnames(df_), "row_id")]
    return( list(IGOID = ItemGroupOID, sdsdata = df_ ))
}
