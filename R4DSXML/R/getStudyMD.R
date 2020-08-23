getStudyMD <- function(filepath) {
        doc <- xmlTreeParse(filepath, useInternalNodes = T)
        namespaces <- namespaces(doc)
        
        StudyInfo <- c()
        
        StudyInfoNodeset <- getNodeSet(doc, "//ns:GlobalVariables/ns:StudyName", namespaces)
        StudyName <- xmlValue(StudyInfoNodeset)
        StudyInfo <- c(StudyInfo, StudyName)

        StandardVersionNodeset <- getNodeSet(doc, "//ns:MetaDataVersion", namespaces)
        StandardVersion <- getAttr(StandardVersionNodeset,"def:StandardVersion")
        StudyInfo <- c(StudyInfo, StandardVersion)
        
        # StandardVersion <- getAttr(StandardVersionNodeset,"def:StandardVersion")
        # StudyInfo <- c(StudyInfo, StandardVersion)
        
        
        names(StudyInfo) <- c("StudyName", "StandardVersion")
        
        
        return(StudyInfo)
}
