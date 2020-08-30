getStudyMD <- function(filepath) {
        doc <- xmlTreeParse(filepath, useInternalNodes = T)
        namespaces <- namespaces(doc)
        
        StudyInfo <- c()
        
        # StudyName
        StudyNameNodeset <- getNodeSet(doc, "//ns:GlobalVariables/ns:StudyName", namespaces)
        StudyName <- xmlValue(StudyNameNodeset)
        StudyInfo <- c(StudyInfo, StudyName)
        
        # StudyDescription
        StudyDescriptionNodeset <- getNodeSet(doc, "//ns:GlobalVariables/ns:StudyDescription", namespaces)
        StudyDescription <- xmlValue(StudyDescriptionNodeset)
        StudyInfo <- c(StudyInfo, StudyDescription)
        
        # ProtocolName
        ProtocolNameNodeset <- getNodeSet(doc, "//ns:GlobalVariables/ns:ProtocolName", namespaces)
        ProtocolName <- xmlValue(ProtocolNameNodeset)
        StudyInfo <- c(StudyInfo, ProtocolName)
        
        # DefineVersion
        DefineVersionNodeset <- getNodeSet(doc, "//ns:MetaDataVersion", namespaces)
        DefineVersion <- getAttr(DefineVersionNodeset,"def:DefineVersion")
        StudyInfo <- c(StudyInfo, DefineVersion)
        
        # StandardName
        StandardNameNodeset <- getNodeSet(doc, "//ns:MetaDataVersion", namespaces)
        StandardName <- getAttr(StandardNameNodeset,"def:StandardName")
        StudyInfo <- c(StudyInfo, StandardName)
        
        # StandardVersion
        StandardVersionNodeset <- getNodeSet(doc, "//ns:MetaDataVersion", namespaces)
        StandardVersion <- getAttr(StandardVersionNodeset,"def:StandardVersion")
        StudyInfo <- c(StudyInfo, StandardVersion)
        
        
        names(StudyInfo) <- c("StudyName", 
                              "StudyDescription", 
                              "ProtocolName", 
                              "DefineVersion", 
                              "StandardName", 
                              "StandardVersion")
        
        return(StudyInfo)
}
