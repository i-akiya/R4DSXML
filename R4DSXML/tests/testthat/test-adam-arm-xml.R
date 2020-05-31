library(R4DSXML)
library(testthat)


## Test metadata preparation
define <- "/Volumes/Transcend/develop/Rwork/VerifiyR4DSXML/TestData/ARM/define2-0-0-example-adam-results.xml"

#Get analysis displays metadata
analysis.displays.metadata <- getARDISP(define)

#Get analysis results metadata
analysis.results.metadata <- getAR(define)

# Excute unit tests for import metadata

# Check number of records
test_that("Check number of records", {
  expect_equal(nrow(analysis.displays.metadata), 2)
  expect_equal(nrow(analysis.results.metadata), 3)
})

# Check number of colums
test_that("Check number of colums", {
  expect_equal(ncol(analysis.displays.metadata), 3)
  expect_equal(ncol(analysis.results.metadata), 7)
})

# Check character value
test_that("check imported values", {
  expect_equal(analysis.displays.metadata[2, "ARD_Name"], "Table 14-5.02")
  expect_equal(
    analysis.displays.metadata[1, "ARD_Description"],
    "Primary Endpoint Analysis: ADAS-Cog - Summary at Week 24 - LOCF (Efficacy Population)"
  )
  expect_equal(analysis.displays.metadata[1,"ARD_OID"], "RD.Table_14-3.01")
  expect_equal(analysis.results.metadata[2, "AR_AnalysisReason"], "SPECIFIED IN SAP" )
  expect_equal(analysis.results.metadata[3, "AR_AnalysisPurpose"], "PRIMARY OUTCOME MEASURE" )
  expect_equal(analysis.results.metadata[1, "AR_Description"], "Dose response analysis for ADAS-Cog changes from baseline" )
  expect_equal(analysis.results.metadata[2, "AR_Doc_Description"], "ANCOVA analysis of CHG performed to provide pairwise comparisons among treatment groups and adjusted means; using randomized treatment as class variable and site group as class variable in model and the baseline value as a covariate." )
  expect_equal(analysis.results.metadata[3, "AR_DocRef"], "LF.SAP-SEC-11.2" )
  expect_equal(analysis.results.metadata[1, "AR_Datasets"], "IG.ADQSADAS" )
  expect_equal(analysis.results.metadata[3, "AR_Datasets"], "IG.ADAE, IG.ADSL" )
  
  
  # expect_equal(variable.metadata[variable.metadata$IR_ItemOID=="IT.ADQSADAS.COMP24FL","ID_Label"], "Completers of Week 24 Population Flag")
  # expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.ADQSADAS.AVAL.ACITM01-ACITM14","ID_DataType"], "integer")
  # expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.ADQSADAS.AVAL.ACITM01-ACITM14","ID_Label"], "Analysis Value")
  # expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.ADQSADAS.QSSEQ.ACITM01-ACITM14","ID_SASFieldName"], "QSSEQ")
  # expect_equal(value.metadata[value.metadata$IR_ItemOID=="IT.ADQSADAS.QSSEQ.ACTOT","ID_OriginType"], "Assigned")
})
