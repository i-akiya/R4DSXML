getCodeListItem <- function(doc, ns) {
    namespaces <- ns
    defVersion <-
        tail(str_split(namespaces[["def"]], "/", n = Inf, simplify = FALSE)[[1]], n = 1)
    
    #Select CodeList nodeset including EnumeratedItem
    clNode <-
        getNodeSet(doc, "//ns:CodeList[ns:CodeListItem]", namespaces)
    #Extract Code List OID as Collection
    CL.OIDs <- getAttr(Nodeset = clNode, Attr = "OID")
    CL.Names <- getAttr(Nodeset = clNode, Attr = "Name")
    CL.DataTypes <- getAttr(Nodeset = clNode, Attr = "DataType")
    if (defVersion == "v2.1") {
        CL.IsNonStandard <-
            getAttr(Nodeset = clNode, Attr = "def:IsNonStandard")
    }
    if (defVersion == "v2.1") {
        CL.StandardOID <-
            getAttr(Nodeset = clNode, Attr = "def:StandardOID")
    }
    CL.SASFormatNames <-
        getAttr(Nodeset = clNode, Attr = "SASFormatName")
    if (defVersion == "v2.1") {
        CL.CommentOID <-
            getAttr(Nodeset = clNode, Attr = "def:CommentOID")
    }
    
    
    for (i in 1:length(clNode)) {
        # get attributes of CodeList element
        OID <- CL.OIDs[i]
        Name <- CL.Names[i]
        DataType <- CL.DataTypes[i]
        if (defVersion == "v2.1") {
            IsNonStandard <- CL.IsNonStandard
            StandardOID <- CL.StandardOID
            CommentOID <- CL.CommentOID
        }
        SASFormatName <- CL.SASFormatNames[i]
        
        # get attributes of CodeList Alias
        CodeListAlias <-
            getNodeSet(
                doc,
                str_c("//ns:CodeList[@OID=\"",  OID, "\"]", "/ns:Alias", sep = ""),
                namespaces
            )
        if (length(CodeListAlias) > 0) {
            CodeListCode <- getAttr(Nodeset = CodeListAlias, Attr = "Name")
            CodeListContext <-
                getAttr(Nodeset = CodeListAlias, Attr = "Context")
        } else{
            CodeListCode <- NA
            CodeListContext <- NA
        }
        
        # get CodeListItem
        codelistItems <- getNodeSet(
            doc,
            str_c(
                "//ns:CodeList[@OID=\"",
                OID,
                "\"]",
                "/ns:CodeListItem",
                sep = ""
            ),
            namespaces
        )
        
        CodedValue <-
            getAttr(Nodeset = codelistItems, Attr = "CodedValue")
        #Decode = Decode
        OrderNumber <-
            getAttr(Nodeset = codelistItems, Attr = "OrderNumber")
        Rank <- getAttr(Nodeset = codelistItems, Attr = "Rank")
        ExtendedValue <-
            getAttr(Nodeset = codelistItems, Attr = "def:ExtendedValue")
        
        # get CodeListItem Alias
        CodeListItemCode <- c()
        CodeListItemContext <- c()
        for (j in 1:length(codelistItems)) {
            codelistAlias <- getNodeSet(
                doc,
                str_c(
                    "//ns:CodeList[@OID=\"",
                    OID,
                    "\"]/ns:CodeListItem[",
                    as.character(j) ,
                    "]",
                    "/ns:Alias",
                    sep = ""
                ) ,
                namespaces
            )
            
            if (length(codelistAlias) == 0) {
                CodeListItemCode <- append(CodeListItemCode, NA)
                CodeListItemContext <-
                    append(CodeListItemContext, NA)
            } else{
                CodeListItemCode <-
                    append(CodeListItemCode,
                           getAttr(Nodeset = codelistAlias, Attr = "Name"))
                CodeListItemContext <-
                    append(
                        CodeListItemContext,
                        getAttr(Nodeset = codelistAlias, Attr = "Context")
                    )
            }
        }
        
        # get CodeListItem Decode
        CodeListItemDecode <- c()
        for (k in 1:length(codelistItems)) {
            #paste0("//CodeListItem[", k,  "]/Decode")
            codelistDecode <- getNodeSet(
                doc,
                str_c(
                    "//ns:CodeList[@OID=\"",
                    OID,
                    "\"]/ns:CodeListItem[",
                    as.character(k) ,
                    "]",
                    "/ns:Decode",
                    sep = ""
                ),
                namespaces
            )
            
            if (length(codelistDecode) == 0) {
                CodeListItemDecode <- append(CodeListItemDecode, NA)
            } else{
                CodeListItemDecode <- append(
                    CodeListItemDecode,
                    getVal(Nodeset = codelistDecode, xpath = 'TranslatedText[@xml:lang = "en"]')
                )
            }
        }
        if (defVersion == "v2.1") {
            df <- data.frame(
                OID = OID,
                Name = Name,
                DataType = DataType,
                #IsNonStandard = IsNonStandard,
                #StandardOID = StandardOID,
                #CommentOID = CommentOID,
                SASFormatName = SASFormatName,
                CodeListCode = CodeListCode,
                CodeListContext = CodeListContext,
                CodedValue = CodedValue,
                Decode = CodeListItemDecode,
                OrderNumber = OrderNumber,
                Rank = Rank,
                ExtendedValue = ExtendedValue,
                ItemCode = CodeListItemCode,
                ItemContext = CodeListItemContext,
                stringsAsFactors = FALSE
            )
        } else {
            df <- data.frame(
                OID = OID,
                Name = Name,
                DataType = DataType,
                SASFormatName = SASFormatName,
                CodeListCode = CodeListCode,
                CodeListContext = CodeListContext,
                CodedValue = CodedValue,
                Decode = CodeListItemDecode,
                OrderNumber = OrderNumber,
                Rank = Rank,
                ExtendedValue = ExtendedValue,
                ItemCode = CodeListItemCode,
                ItemContext = CodeListItemContext,
                stringsAsFactors = FALSE
            )
        }
        
        if (i == 1) {
            codelistDF <- df
        } else{
            codelistDF <- rbind(codelistDF, df)
        }
    }
    
    return(codelistDF)
}
