getItemGroupDataSeq <-
function( ItemGroupData ){
    sapply( ItemGroupData, 
            function(el) xmlGetAttr(el, 
            'data:ItemGroupDataSeq', 
            default = ""
            )
    )
}
