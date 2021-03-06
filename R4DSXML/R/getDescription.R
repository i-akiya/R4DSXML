getDescription <- function(Nodeset, namespaces){
    descriptionVec <- c()
    
    for (i in 1:length(Nodeset)){
      descriptionf <- getNodeSet(Nodeset[[i]], "./ns:Description", namespaces)
      
      if(length(descriptionf) < 1){
        descriptionVec <- append(descriptionVec, NA)
      }else{
        descriptionVec_ <- getVal(descriptionf, 'TranslatedText[@xml:lang = "en"]')
        if(descriptionVec_ == ""){
          descriptionVec <- append(descriptionVec, NA)
        }else{
          descriptionVec <- append(descriptionVec, descriptionVec_) 
        }
        
      }
    }
    
    return(descriptionVec)
}