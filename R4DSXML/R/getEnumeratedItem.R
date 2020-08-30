getEnumeratedItem <- function(doc, ns) {
    namespaces <- ns
    defVersion <-
        tail(str_split(namespaces[["def"]], "/", n = Inf, simplify = FALSE)[[1]], n = 1)
    
    #Select CodeList nodeset including CodeListItem and EnumeratedItem
    clNode <-
        getNodeSet(doc, "//ns:CodeList[ns:EnumeratedItem]", namespaces)
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
        
        CodeListAlias <- getNodeSet(
            doc,
            str_c("//ns:CodeList[@OID=\"",  OID, "\"]", "/ns:Alias[@Context=\"nci:ExtCodeID\"]", sep = ""),
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
        
        # get EnumeratedItem
        enumItems <- getNodeSet(
            doc,
            str_c(
                "//ns:CodeList[@OID=\"",
                OID,
                "\"]",
                "/ns:EnumeratedItem",
                sep = ""
            ),
            namespaces
        )
        
        CodedValue <-
            getAttr(Nodeset = enumItems, Attr = "CodedValue")
        OrderNumber <-
            getAttr(Nodeset = enumItems, Attr = "OrderNumber")
        Rank <- getAttr(Nodeset = enumItems, Attr = "Rank")
        ExtendedValue <-
            getAttr(Nodeset = enumItems, Attr = "def:ExtendedValue")
        
        # get EnumeratedItem Alias
        EnumeratedItemCode <- c()
        EnumeratedItemContext <- c()
        for (j in 1:length(enumItems)) {
            enumAlias <- getNodeSet(
                doc,
                str_c(
                    "//ns:CodeList[@OID=\"",
                    OID,
                    "\"]/ns:EnumeratedItem[",
                    as.character(j) ,
                    "]",
                    "/ns:Alias[@Context=\"nci:ExtCodeID\"]",
                    sep = ""
                ),
                namespaces
            )
            
            if (length(enumAlias) == 0) {
                EnumeratedItemCode <- append(EnumeratedItemCode, NA)
                EnumeratedItemContext <-
                    append(EnumeratedItemContext, NA)
            } else {
                EnumeratedItemCode <-
                    append(EnumeratedItemCode,
                           getAttr(Nodeset = enumAlias, Attr = "Name"))
                EnumeratedItemContext <-
                    append(
                        EnumeratedItemContext,
                        getAttr(Nodeset = enumAlias, Attr = "Context")
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
                OrderNumber = OrderNumber,
                Rank = Rank,
                ExtendedValue = ExtendedValue,
                ItemCode = EnumeratedItemCode,
                ItemContext = EnumeratedItemContext,
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
                OrderNumber = OrderNumber,
                Rank = Rank,
                ExtendedValue = ExtendedValue,
                ItemCode = EnumeratedItemCode,
                ItemContext = EnumeratedItemContext,
                stringsAsFactors = FALSE
            )
        }
        
        if (i == 1) {
            enumDF <- df
        } else{
            enumDF <- rbind(enumDF, df)
        }
    }
    
    
    return(enumDF)
}
