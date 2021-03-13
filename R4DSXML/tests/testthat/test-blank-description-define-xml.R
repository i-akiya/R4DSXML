library(R4DSXML)
library(testthat)


## Test metadata preparation 
define <- "/Users/ippei/develop/data/cdisc/updated_cdiscpilot/UpdatedCDISCPILOT-define.xml"


#Get variable metadata
var.md <- R4DSXML::getVarMD(define)

#Get value level metadata
val.md <- R4DSXML::getValMD(define)


# Check Variable Metadata
test_that("check a blank description in the variable metadata", {
    expect_equal(length( var.md[, is.na(var.md$ID_Label)] ), 0)
})

# Check Value Level Metadata
test_that("check a blank description in the value level metadata", {
    expect_equal(length( val.md[, is.na(val.md$ID_Label)==F] ), 0)
})


