getCodeListItem <- function(filepath){
    doc = xmlTreeParse( filepath, useInternalNodes = T )
    ns <- namespaces()
    
    #Select CodeList nodeset including EnumeratedItem
    clNode <- getNodeSet( doc, "//ns:CodeList[ns:CodeListItem]", ns )
    #Extract Code List OID as Collection
    CL.OIDs <- getAttr( Nodeset = clNode, Attr = "OID" )
    CL.Names <- getAttr( Nodeset = clNode, Attr = "Name" )
    CL.DataTypes <- getAttr( Nodeset = clNode, Attr = "DataType" )
    CL.SASFormatNames <- getAttr( Nodeset = clNode, Attr = "SASFormatName" )
    
    
    for ( i in 1:length(clNode) ){
        # get attributes of CodeList element
        OID <- CL.OIDs[i]
        Name <- CL.Names[i]
        DataType <- CL.DataTypes[i]
        SASFormatName <- CL.SASFormatNames[i]
        
        # get attributes of CodeList Alias
        doc2 <- xmlTreeParse(toString.XMLNode( clNode[[i]] ), useInternalNodes = T)
        CodeListAlias <- getNodeSet( doc2, "/CodeList/Alias", ns )
        if (length(CodeListAlias) > 0){
            CodeListCode <- getAttr( Nodeset = CodeListAlias, Attr = "Name" )
            CodeListContext <- getAttr( Nodeset = CodeListAlias, Attr = "Context" )
        }else{
            CodeListCode <- NA
            CodeListContext <- NA
        }
        
        # get CodeListItem
        codelistItems <- getNodeSet( doc2, "//CodeListItem", ns )
        
        CodedValue <- getAttr( Nodeset = codelistItems, Attr = "CodedValue" )
        #Decode = Decode
        OrderNumber <- getAttr( Nodeset = codelistItems, Attr = "OrderNumber" )
        Rank <- getAttr( Nodeset = codelistItems, Attr = "Rank" )
        ExtendedValue <- getAttr( Nodeset = codelistItems, Attr = "def:ExtendedValue" )
        
        # get CodeListItem Alias
        CodeListItemCode <- c()
        CodeListItemContext <- c()
        for (j in 1:length(codelistItems)){
            codelistAlias <- getNodeSet( doc2, paste0("//CodeListItem[", j,  "]/Alias"), ns  )
            if( length(codelistAlias)==0 ){
                CodeListItemCode <- append(CodeListItemCode, NA)
                CodeListItemContext <- append(CodeListItemContext, NA)
            }else{
                CodeListItemCode <- append(CodeListItemCode, getAttr( Nodeset = codelistAlias, Attr = "Name" ))
                CodeListItemContext <- append(CodeListItemContext, getAttr( Nodeset = codelistAlias, Attr = "Context" ))
            }
        }
        
        # get CodeListItem Decode
        CodeListItemDecode <- c()
        for (k in 1:length(codelistItems)){
            codelistDecode <- getNodeSet( doc2, paste0("//CodeListItem[", k,  "]/Decode") , ns )
            if( length(codelistDecode)==0 ){
                CodeListItemDecode <- append(CodeListItemDecode, NA)
            }else{
                CodeListItemDecode <- append(CodeListItemDecode, getVal( Nodeset = codelistDecode, xpath = 'TranslatedText[@xml:lang = "en"]' ))
            }
        }
        
        df <- data.frame(OID=OID, 
                         Name=Name, 
                         DataType=DataType, 
                         SASFormatName=SASFormatName, 
                         CodeListCode=CodeListCode, 
                         CodeListContext=CodeListContext,
                         CodedValue=CodedValue, 
                         Decode=CodeListItemDecode,
                         OrderNumber=OrderNumber, 
                         Rank=Rank, 
                         ExtendedValue=ExtendedValue,
                         ItemCode=CodeListItemCode,
                         ItemContext=CodeListItemContext,
                         stringsAsFactors=FALSE)
        
        if ( i ==1 ){
            codelistDF <- df
        }else{
            codelistDF <- rbind(codelistDF, df)
        }
    }
    
    return(codelistDF)
}

