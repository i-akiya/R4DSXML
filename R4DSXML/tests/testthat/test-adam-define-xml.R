library(R4DSXML)
library(testthat)


## Test metadata preparation 
define <- system.file("extdata",
                      "ADaM",
                      "define2-0-0-example-adam.xml", 
                      package="R4DSXML") 

#Get dataset level metadata
dataset.metadata <- getDLMD(define)

#Get variable level metadata
variable.metadata <- getVarMD(define)

#Get value level metadata
value.metadata <- getValMD(define)

#Get Controlled Terminology
ct.metadata <- getCT(define)

# Excute unit tests for import metadata

# Check number of records
test_that("Check number of records", {
    expect_equal(nrow(dataset.metadata), 2)
    expect_equal(nrow(variable.metadata[variable.metadata$IGD_Name == "ADSL",]), 48)
    expect_equal(nrow(value.metadata[value.metadata$ValueListOID == "VL.ADQSADAS.AVAL",]), 2)
})

# Check number of colums
test_that("Check number of colums", {
    expect_equal(ncol(dataset.metadata), 12)
    expect_equal(ncol(variable.metadata), 16)
    expect_equal(ncol(value.metadata), 15)
})

# Check character value 
test_that("check imported values", {
    expect_equal(dataset.metadata[1,"IGD_Name"], "ADSL")
    expect_equal(dataset.metadata[2,"IGD_Structure"], "One record per subject per parameter per analysis visit per analysis date")
    expect_equal(variable.metadata[variable.metadata$IR_ItemOID=="IT.ADSL.AGE","ID_DataType"], "integer" )
    expect_equal(variable.metadata[variable.metadata$IR_ItemOID=="IT.ADQSADAS.COMP24FL","ID_Label"], "Completers of Week 24 Population Flag")
    expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.ADQSADAS.AVAL.ACITM01-ACITM14","ID_DataType"], "integer")
    expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.ADQSADAS.AVAL.ACITM01-ACITM14","ID_Label"], "Analysis Value")
    expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.ADQSADAS.QSSEQ.ACITM01-ACITM14","ID_SASFieldName"], "QSSEQ")
    expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.ADQSADAS.QSSEQ.ACTOT","ID_OriginType"], "Assigned")
})


