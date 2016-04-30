getOrigin <- function(Nodeset){
    namespaces <- namespaces()
    origintypeVec <- c()
    origindescVec <- c()
    
    for (i in 1:length(Nodeset)){
      originf <- getNodeSet(Nodeset[[i]], "./def:Origin", namespaces)
      
      if(length(originf) < 1){
        origintypeVec <- append(origintypeVec, NA)
        origindescVec <- append(origindescVec, NA)
        
      }else{
        origintypeVec <- append(origintypeVec, getAttr(Nodeset=originf, Attr="Type"))
        origindescVec_ <- getVal(originf, 'TranslatedText[@xml:lang = "en"]')
        if(origindescVec_ == ""){
          origindescVec <- append(origindescVec, NA)
        }else{
          origindescVec <- append(origindescVec, origindescVec_) 
        }
        
      }
    }
    
    originList <- list(origintypeVec, origindescVec)
    
    return(originList)
}