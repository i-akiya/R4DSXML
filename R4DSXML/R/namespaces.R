namespaces <- function(doc){
        rootdoc <- xmlRoot(doc)
        ns <- xmlNamespaceDefinitions(rootdoc, simplify = TRUE)
        namelist <- names(ns)
        namelist[str_length(namelist)==0] <- "ns"
        names(ns) <- namelist
        rm(namelist)
        
        return(ns)
}