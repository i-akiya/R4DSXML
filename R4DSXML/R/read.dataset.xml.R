read.dataset.xml <-
    function( dataset_xml, define_xml ){      
        doc = xmlTreeParse(dataset_xml, useInternalNodes = T)
        sds <- getSDS( doc )
        ItemGroupOID <- sds$IGOID
        df <- sds$sdsdata
        
                
        # set variable order    
        variable.metadata <- getVarMD( define_xml )
        variable.metadata_ <- subset(variable.metadata, 
                                 variable.metadata$IGD_Name == unlist( strsplit(ItemGroupOID, "[.]" ))[2]
                                 #select = c( ID_Name, IR_OrderNumber )
                              )
        variable.metadata_ <- data.frame(
                                variable.metadata_$ID_Name, 
                                variable.metadata_$IR_OrderNumber,
                                stringsAsFactors = FALSE,
                                row.names = NULL
                                )
        names( variable.metadata_ ) <- c( "ID_Name", "IR_OrderNumber" )
        
        n.vars <- length( names( df ) )
        originalSeq <- c(1:n.vars)
        domain2Seq <- data.frame( originalSeq, 
                                  Variable.Name = names( df ), 
                                  stringsAsFactors = FALSE,
                                  row.names = NULL)
        
        varSeq <- merge(variable.metadata_, domain2Seq, by.x = "ID_Name", by.y = "Variable.Name")
        varSeq <- varSeq[order(varSeq$IR_OrderNumber),]
        
        df <- df[,varSeq$originalSeq]
        # end 
        
        # set variable type
        type.subset <- 
            subset(variable.metadata, 
                  variable.metadata$IGD_Name == unlist( strsplit(ItemGroupOID, "[.]" ))[2]
                   
            )
      
        variable.metadata.type <- data.frame( as.character(type.subset$ID_Name), 
                                               as.character(type.subset$ID_DataType), 
                                               stringsAsFactors = FALSE,
                                               row.names = NULL)
        colnames( variable.metadata.type ) <- c( "ID_Name", "ID_DataType" )
        
        for ( i in c(1:nrow(variable.metadata.type)) ) {
            if ( variable.metadata.type[i, "ID_DataType"] == "integer" ){
              var_ <- variable.metadata.type[i, "ID_Name"]
              eval( parse( text = paste("df$", var_, " <- ",  "as.integer(df$", var_, ")", sep="")))
            }
            else if ( variable.metadata.type[i, "ID_DataType"] == "float" ){
              var_ <- variable.metadata.type[i, "ID_Name"]
              eval( parse( text = paste("df$", var_, " <- ",  "as.double(df$", var_, ")", sep="")))
            }
        }
    row.names(df) <- NULL
    return(df)
}
