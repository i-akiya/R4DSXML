getVal <- function(Nodeset, xpath){
    sapply(Nodeset, function(el) xmlValue(el, xpath))
}