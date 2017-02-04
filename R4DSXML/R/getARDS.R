getARDS <- function( filepath, AnalysisResultOIDs ) {
    doc = xmlTreeParse( filepath, useInternalNodes = T )
    namespaces <- namespaces()
    
    dsdf <- data.frame()
    
    for( i in AnalysisResultOIDs ){
      xpath_str <- paste0("//arm:AnalysisResultDisplays/arm:ResultDisplay/arm:AnalysisResult[@OID=\"", i, "\"]/arm:AnalysisDatasets/arm:AnalysisDataset")
      aaa <- getNodeSet(doc, xpath_str, namespaces)
      ds_vec <- getAttr( Nodeset = aaa,  Attr="ItemGroupOID")
      ds_str <- paste(ds_vec, collapse=", ")
      dsdf_ <- data.frame( i, ds_str, stringsAsFactors = FALSE)
      dsdf <- rbind(dsdf, dsdf_)
    }
    
    names(dsdf) <- c("AR_OID", "AR_Datasets")
    
    return( dsdf )
}
