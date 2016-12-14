getValueListRef <- function(Nodeset){
  namespaces <- namespaces()
  clfVec <- c()
  
  for (i in 1:length(Nodeset)){
    clf <- getNodeSet(Nodeset[[i]], "./def:ValueListRef", namespaces)
    
    if(length(clf) < 1){
      clfVec <- append(clfVec, NA)
    }else{
      clfVec <- append(clfVec, getAttr(Nodeset=clf, Attr="ValueListOID"))
    }
  }
  return(clfVec)
}