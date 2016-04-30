getAttr <- function(Nodeset, Attr){
    sapply(Nodeset, function(el) xmlGetAttr(el, Attr, default = NA))
}
