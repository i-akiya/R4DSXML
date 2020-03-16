library(R4DSXML)
library(testthat)


## Test metadata preparation 
define <- system.file("extdata",
                      "define-xml-2.1",
                      "defineV21-SDTM.xml", 
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
    expect_equal(nrow(dataset.metadata), 11)
    expect_equal(nrow(dataset.metadata), 11)
  
  
#    expect_equal(nrow(variable.metadata[variable.metadata$IGD_Name == "LB",]), 28)
#    expect_equal(nrow(value.metadata[value.metadata$ValueListOID == "VL.QS.QSORRES",]), 28)
})

# Check number of colums
test_that("Check number of colums", {
    expect_equal(ncol(dataset.metadata), 12)
    expect_equal(ncol(variable.metadata), 15)
    expect_equal(ncol(value.metadata), 14)
})

# Check character value 
test_that("chech an imported value", {
    expect_equal(dataset.metadata[3,"IGD_Name"], "TI")
    expect_equal(dataset.metadata[8,"IGD_Structure"], "One record per actual visit per subject")
    expect_equal(variable.metadata[variable.metadata$IR_ItemOID=="IT.EG.EGDTC","ID_DataType"], "date" )
    expect_equal(variable.metadata[variable.metadata$IR_ItemOID=="IT.DS.DSCAT","IR_Mandatory"], "No")
    expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.SC.SCORRES.MARISTAT","ID_DataType"], "text")
    expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.VS.VSORRESU.WEIGHT.DM.COUNTRY.CMETRIC","ID_SASFieldName"], "WEIGHTU")
})

# Check integer value 
test_that("chech an imported value", {
  expect_equal(variable.metadata[variable.metadata$IR_ItemOID=="IT.SE.SESEQ","IR_OrderNumber"], 4 )
  expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.EG.EGORRES.QRSDUR","IR_OrderNumber"], 3)
})


# Check code list value 
test_that("check the controlled terminology", {
    expect_equal(nrow(ct.metadata[ct.metadata$OID=="CL.MARISTAT", ]), 6 )
    expect_equal(nrow(ct.metadata[ct.metadata$OID=="CL.TREASFF", ]), 11)
    expect_equal(ct.metadata[ct.metadata$CodedValue=="Clinical Study Sponsor", "ItemCode"], "C70793")
    expect_equal(ct.metadata[ct.metadata$CodedValue=="INSUFFICIENT RESPONSE", "OrderNumber"], "3")
    expect_equal(ct.metadata[ct.metadata$CodedValue=="SEVERE", "Rank"], "3")
    expect_equal(ct.metadata[ct.metadata$CodedValue=="MARISTAT", "Decode"], "Marital Status")
})




