getDLMD <- function(filepath) {
    doc = xmlTreeParse(filepath, useInternalNodes = T)
    namespaces <- c(ns='http://www.cdisc.org/ns/odm/v1.3')
    
    #Dataset Level Metadata
    ItemGroupDef <- 
        getNodeSet(doc, "//ns:ItemGroupDef", namespaces)
    aryAttr <- c("OID", "Domain", "Name", "Repeating", "IsReferenceData",
                 "SASDatasetName", "Purpose", "Structure", "Class", "ArchiveLocationID")
    
    IGD_OID <- getAttr(Nodeset=ItemGroupDef, Attr="OID")
    IGD_Domain <- getAttr(Nodeset=ItemGroupDef, Attr="Domain")
    IGD_Name <- getAttr(Nodeset=ItemGroupDef, Attr="Name")
    IGD_Repeating <- getAttr(Nodeset=ItemGroupDef, Attr="Repeating")
    IGD_IsReferenceData <- getAttr(Nodeset=ItemGroupDef, Attr="IsReferenceData")
    IGD_SASDatasetName <- getAttr(Nodeset=ItemGroupDef, Attr="SASDatasetName")
    IGD_Purpose <- getAttr(Nodeset=ItemGroupDef, Attr="Purpose")
    IGD_Structure <- getAttr(Nodeset=ItemGroupDef, Attr="Structure")
    IGD_Class <- getAttr(Nodeset=ItemGroupDef, Attr="Class")
    IGD_ArchiveLocationID <- getAttr(Nodeset=ItemGroupDef, Attr="ArchiveLocationID")
    
    
    IGD <- getNodeSet(doc, "//ns:ItemGroupDef//ns:Description", namespaces)
    IGD_Description <- 
        sapply(IGD, function(el) xmlValue(el, 'Description/TranslatedText[@xml:lang = "en"]')
               )
    IGD_title <- 
        sapply(IGD, function(el) xmlValue(el, "def:leaf/def:title")
               )
    
    df <- data.frame(IGD_OID, 
                     IGD_Domain, 
                     IGD_Name, 
                     IGD_Repeating, 
                     IGD_IsReferenceData,
                     IGD_SASDatasetName,
                     IGD_Purpose,
                     IGD_Structure,
                     IGD_Class,
                     IGD_ArchiveLocationID,
                     IGD_Description,
                     IGD_title,
                     stringsAsFactors = FALSE)
    row.names(df) <- NULL
    return(df)
}
