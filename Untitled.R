define2.0 <- system.file("extdata",
                         "ADaM",
                         "define2-0-0-example-adam.xml", 
                         package="R4DSXML") 
#Get dataset level metadata
dataset.metadata <- getDLMD(define2.0)
variable.metadata <- getVarMD(define2.0)
value.metadata <- getValMD(define2.0)
ct.metadata <- getCT(define2.0)



define2.1 <- system.file("extdata",
                      "define-xml-2.1",
                      "defineV21-SDTM.xml", 
                      package="R4DSXML") 
#Get dataset level metadata
dataset.metadata <- getDLMD(define2.1)
variable.metadata <- getVarMD(define2.1)
value.metadata <- getValMD(define2.1)
ct.metadata <- getCT(define2.1)


doc = xmlTreeParse( define, useInternalNodes = T )
getAttr <- function(Nodeset, Attr){
        sapply(Nodeset, function(el) xmlGetAttr(el, Attr, default = NA))
}
namespaces <- function(doc){
        rootdoc <- xmlRoot(doc)
        ns <- xmlNamespaceDefinitions(rootdoc, simplify = TRUE)
        namelist <- names(ns)
        namelist[str_length(namelist)==0] <- "ns"
        names(ns) <- namelist
        rm(namelist)
        
        return(ns)
}
namespaces <- namespaces(doc)
clNode <- getNodeSet( doc, "//ns:CodeList[@OID=\"CL.DI.DOMAIN\"]/ns:CodeListItem", namespaces )

xmlNode("ODM",
        namespaceDefinitions = namespaces)



getNodeSet( doc, str_c("//ns:CodeList[@OID=\"", "CL.DI.DOMAIN", "\"]", "/Alias", sep = ""), namespaces )
getNodeSet( doc, str_c("//ns:CodeList[@OID=\"CL.DI.DOMAIN\"]/ns:Alias", sep = ""), namespaces )

