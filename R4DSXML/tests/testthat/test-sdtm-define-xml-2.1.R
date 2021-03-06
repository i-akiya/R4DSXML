library(R4DSXML)
library(testthat)


## Test metadata preparation 
define <- "/Users/ippei/develop/data/cdisc/DefineV21ReleasePackage/DefineV21/examples/DefineXML-2-1-SDTM/defineV21-SDTM.xml"

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
    expect_equal(nrow(variable.metadata), 155)
    expect_equal(nrow(value.metadata), 44)
    expect_equal(nrow(ct.metadata), 162)
})

# Check number of colums
test_that("Check number of colums", {
    expect_equal(ncol(dataset.metadata), 12)
    expect_equal(ncol(variable.metadata), 16)
    expect_equal(ncol(value.metadata), 15)
    expect_equal(ncol(ct.metadata), 13)
})

# Check specific colum
test_that("Check exist of specific colums", {
  expect_false( "ID_SASFormatName" %in% names(variable.metadata) )
  expect_true( "ID_DisplayFormat" %in% names(variable.metadata) )


})

# Check character value 
test_that("chech an imported value", {
    expect_equal(dataset.metadata[3,"IGD_Name"], "DM")
    expect_equal(dataset.metadata[8,"IGD_Structure"], "One record per finding per visit per subject")
    expect_equal(dataset.metadata[dataset.metadata$IGD_OID=="IG.TS","IGD_Class"], "TRIAL DESIGN")
    expect_equal(variable.metadata[variable.metadata$IR_ItemOID=="IT.VS.VSDTC","ID_DataType"], "date" )
    expect_equal(variable.metadata[variable.metadata$IR_ItemOID=="IT.LB.LBCAT","IR_Mandatory"], "No")
    expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.TS.TSVAL.AGEMIN","ID_DataType"], "integer")
    expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.VS.VSORRESU.WEIGHT.DM.COUNTRY.CMETRIC","ID_SASFieldName"], "WEIGHTOM")
    
})

# Check integer value 
test_that("chech an imported value", {
  expect_equal(variable.metadata[variable.metadata$IR_ItemOID=="IT.TS.TSSEQ","IR_OrderNumber"], 3 )
  expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.LB.LBORRES.VITB9.LBSPEC.BLOOD","ID_SignificantDigits"], 1)
})


# Check code list value 
test_that("check the controlled terminology", {
    expect_equal(nrow(ct.metadata[ct.metadata$OID=="CL.EXTRT", ]), 2 )
    expect_equal(ct.metadata[ct.metadata$CodedValue=="Clinical Study Sponsor", "ItemCode"], "C70793")
    expect_equal(ct.metadata[ct.metadata$CodedValue=="BLACK OR AFRICAN AMERICAN", "OrderNumber"], "3")
    expect_equal(ct.metadata[ct.metadata$CodedValue=="MEDIUM", "Rank"], "2")
    expect_equal(ct.metadata[ct.metadata$CodedValue=="UNDIFFERENTIATED", "Decode"], "Undifferentiated")
})


# Check code list value 
test_that("check the dataset class", {
  expect_equal(nrow(ct.metadata[ct.metadata$OID=="CL.EXTRT", ]), 2 )
  expect_equal(ct.metadata[ct.metadata$CodedValue=="Clinical Study Sponsor", "ItemCode"], "C70793")
  expect_equal(ct.metadata[ct.metadata$CodedValue=="BLACK OR AFRICAN AMERICAN", "OrderNumber"], "3")
  expect_equal(ct.metadata[ct.metadata$CodedValue=="MEDIUM", "Rank"], "2")
  expect_equal(ct.metadata[ct.metadata$CodedValue=="UNDIFFERENTIATED", "Decode"], "Undifferentiated")
})
"TRIAL DESIGN"


