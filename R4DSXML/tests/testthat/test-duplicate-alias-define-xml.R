library(R4DSXML)
library(testthat)


## Test metadata preparation 
define <- "/Users/ippei/develop/data/cdisc/DuplicateAlias/define2-0-0-example-sdtm.xml"


#Get Controlled Terminology
ct.metadata <- getCT(define)



# Check code list value 
test_that("check the controlled terminology", {
    expect_equal(nrow(ct.metadata[ct.metadata$OID=="CL.MARISTAT", ]), 6 )
    expect_equal(nrow(ct.metadata[ct.metadata$OID=="CL.TREASFF", ]), 11)
    expect_equal(ct.metadata[ct.metadata$CodedValue=="Clinical Study Sponsor", "ItemCode"], "C70793")
    expect_equal(ct.metadata[ct.metadata$CodedValue=="INSUFFICIENT RESPONSE", "OrderNumber"], "3")
    expect_equal(ct.metadata[ct.metadata$CodedValue=="SEVERE", "Rank"], "3")
    expect_equal(ct.metadata[ct.metadata$CodedValue=="MARISTAT", "Decode"], "Marital Status")
})




