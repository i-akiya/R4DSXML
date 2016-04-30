library(R4DSXML)
library(testthat)



## Test data preparation 
define <- system.file("extdata", 
                      "SDTM",
                      "define2-0-0-example-sdtm(2013-11-09).xml", 
                      package="R4DSXML"
) 

# Read DM file
sds_dm <- system.file("extdata", 
                      "SDTM",
                   "dm.xml", 
                   package="R4DSXML"
)

DM <- read.dataset.xml(dataset_xml=sds_dm, define_xml=define)


# Read AE file
sds_ae <- system.file("extdata", 
                      "SDTM",
                      "ae.xml", 
                      package="R4DSXML" )
AE <- read.dataset.xml(dataset_xml=sds_ae, define_xml=define)


# Read LB file
sds_lb <- system.file("extdata", 
                      "SDTM",
                      "lb.xml", 
                      package="R4DSXML" )
LB <- read.dataset.xml(dataset_xml=sds_lb, define_xml=define)


# Read TV file
sds_tv <- system.file("extdata", 
                      "SDTM",
                      "tv.xml", 
                      package="R4DSXML")
TV <- read.dataset.xml(dataset_xml=sds_tv, define_xml=define)


# Excute unit tests for import Dataset-XML

# Check number of records
test_that("Check number of records", {
    expect_equal(nrow(DM), 5)
    expect_equal(nrow(AE), 16)
    expect_equal(nrow(TV), 4)
})


# Check number of colums
test_that("Check number of colums", {
    expect_equal(ncol(DM), 16)
    expect_equal(ncol(AE), 18)
    expect_equal(ncol(TV), 6)
})


# Check culumn name
test_that("Check a column name", {
    expect_equal(names(DM)[5], "RFSTDTC")
    expect_equal(names(AE)[10], "AESEV")
    expect_equal(names(TV)[5], "VISITDY")
})


# Check character value 
test_that("check an imported value", {
    expect_equal(DM[3,"USUBJID"], "CDISC01.200001")
    expect_equal(DM[4,"ETHNIC"], "NOT HISPANIC OR LATINO")
    expect_equal(AE[5,"AETERM"], "HEMORRHOIDS")
    expect_equal(AE[15,"AEBODSYS"], "Musculoskeletal and connective tissue disorders")
    expect_equal(TV[1,"VISIT"], "SCREEN")
    expect_equal(TV[3,"TVSTRL"], "End of week 2 treatment")
})


# Check integer value 
test_that("chech integer value", {
    expect_equal(DM[3,"AGE"], 80)
    expect_equal(DM[5,"AGE"], 66)
    expect_equal(AE[8,"AESEQ"], 5)
    expect_equal(AE[16,"AESTDY"], 88)
    expect_equal(TV[2,"VISITNUM"], 2)
    expect_equal(TV[4,"VISITDY"], 169)
})


# Check float value 
test_that("chech float value", {
    expect_equal(LB[21,"LBSTRESN"], 0.6)
    expect_equal(LB[39,"LBSTNRHI"], 1.7)
})
