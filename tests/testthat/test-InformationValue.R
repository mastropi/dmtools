# Created:      04-Aug-2021
# Author:       Daniel Mastropietro
# Description:  Test the dmtools package
#

library(testthat)
library(dmtools)
context("Information Value")


#-- Setup
# Datasets
data(CO2)
# Neede when running the tests by sourcing this file
dat = try( read.csv("./data/Familiar-1000obs.csv"), silent=TRUE )
if (inherits(dat, "try-error")) {
  # Needed when running the tests in RStudio
  dat = read.csv("../../data/Familiar-1000obs.csv")
}

# Variables for dat
target_bin = "B_TARGET"
target_cont_pos = "N_CLI_EDAD"  # This variable takes all non-negative values
target_cont = "N_CLI_PA_DDM_ACTUAL_MAX" # This variable can take negative values

varclass = "
B_TARGET
I_PA_AREA
I_PA_PRODUCTO
"
varnum = "
N_CLI_EDAD
N_PA_ANTIG
N_CLI_PA_DDM_ACTUAL_MAX
P_CLI_TC_DEUDA_v_CREDITO
"
## NOTE: The last two variables, N_CLI_PA_DDM_ACTUAL_MAX and P_CLI_TC_DEUDA_v_CREDITO, take negative values as well,
## so it's good for testing negative values in input variables.


#----------------------------------------- Binary target --------------------------------------
test_that("The IV for a BINARY target variable on both categorical and continuous variables is as expected", {
  dat.iv = InformationValue(
    data=dat,
    target=target_bin,
    varclass=varclass,
    varnum=varnum,
    stat="median",
    groups=16
  )
  # Data structure generated with dput(dat.iv$IV)
  expected_iv = structure(list(var = c("B_TARGET", "N_CLI_PA_DDM_ACTUAL_MAX", "P_CLI_TC_DEUDA_v_CREDITO", "N_CLI_EDAD", "N_PA_ANTIG", "I_PA_PRODUCTO", "I_PA_AREA"),
                               vartype = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character"),
                               type = c("categorical", "continuous", "continuous", "continuous", "continuous", "categorical", "categorical"),
                               nobs = c(1000, 1000, 1000, 1000, 1000, 1000, 1000),
                               nlevels = c(2, 14, 15, 16, 13, 4, 3),
                               TotalWOE = c(1.905, -6.552, -23.51, -8.236, 0.3188, -3.05, 0.8715),
                               IV = c(20.84, 1.663, 0.9898, 0.8045, 0.1228, 0.06888, 0.02266),
                               IVAdj = c(83.34, 1.747, 1.013, 0.8045, 0.1328, 0.1378, 0.05718)),
                          row.names = c(1L, 6L, 7L, 4L, 5L, 3L, 2L), class = "data.frame")
  expect_equal(dat.iv$IV, expected_iv)
})

test_that("The WOE and IV of a variable with NO information is 0 (WOE table is checked as well)", {
  CO2.iv = InformationValue(
    data=CO2,
    target="Treatment",
    varclass="Type"
  )
  expected_woe = structure(list(var = c("Type", "Type", "--TOTAL--"),
                                vartype = c("character", "character", "---------"),
                                type = c("categorical", "categorical", "-----------"),
                                group = c("Quebec", "Mississippi", NA),
                                nobs = c(42, 42, 84),
                                pctClassFIRST = c(50, 50, 50),
                                pctClassLAST = c(50, 50, 50),
                                mean = c(NA_real_, NA_real_, NA_real_),
                                woe = c(0, 0, 0),
                                iv = c(0, 0, 0)),
                           row.names = c(NA, 3L), class = "data.frame")
  expected_iv = structure(list(var = "Type", vartype = "character", type = "categorical",
                               nobs = 84, nlevels = 2, TotalWOE = 0, IV = 0, IVAdj = 0),
                          row.names = 1L, class = "data.frame")
  expect_equal(CO2.iv$WOE, expected_woe)
  expect_equal(CO2.iv$IV, expected_iv)
})

test_that("The IV of a variable with full discrimination on a binary target is very LARGE (i.e. >> 1)", {
  CO2.iv = InformationValue(
    data=CO2,
    target="Treatment",
    varclass="Plant"
  )
  expected_iv = structure(list(var = "Plant", vartype = "character", type = "categorical",
                               nobs = 84, nlevels = 12, TotalWOE = 0, IV = 13.1, IVAdj = 15.8),
                          row.names = 1L, class = "data.frame")
  expect_equal(CO2.iv$IV, expected_iv)
})
#----------------------------------------- Binary target --------------------------------------



#--------------------------------------- Continuous target ------------------------------------
test_that("The IV for a NON-NEGATIVE CONTINUOUS target variable on both categorical and continuous variables is as expected", {
  dat.iv = InformationValue(
    data=dat,
    target=target_cont_pos,
    varclass=varclass,
    varnum=varnum,
    stat="median",
    groups=16
  )
  # Data structure generated with dput(dat.iv$IV)
  expected_iv = structure(list(var = c("N_CLI_EDAD", "P_CLI_TC_DEUDA_v_CREDITO", "N_CLI_PA_DDM_ACTUAL_MAX", "B_TARGET", "I_PA_PRODUCTO", "N_PA_ANTIG", "I_PA_AREA"),
                               vartype = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character"),
                               type = c("continuous", "continuous", "continuous", "categorical", "categorical", "continuous", "categorical"),
                               nobs = c(1000, 1000, 1000, 1000, 1000, 1000, 1000),
                               nlevels = c(16, 15, 14, 2, 4, 13, 3),
                               TotalWOE = c(-2.764, 1.632, -0.5109, -0.202, -0.4109, -0.1344, 0.3695),
                               IV = c(0.4556, 0.02077, 0.01803, 0.007014, 0.006932, 0.002975, 0.002091),
                               IVAdj = c(0.4556, 0.02126, 0.01895, 0.02806, 0.01386, 0.003216, 0.005276)),
                          row.names = c(4L, 7L, 6L, 1L, 3L, 5L, 2L), class = "data.frame")
  expect_equal(dat.iv$IV, expected_iv)
})

test_that("The IV on a GENERAL CONTINUOUS target variable (with both negative and positive values)
          on both categorical and continuous variables is as expected", {
  dat.iv = InformationValue(
    data=dat,
    target=target_cont,
    varclass=varclass,
    varnum=varnum,
    stat="median",
    groups=16
  )
  # Data structure generated with dput(dat.iv$IV)
  expected_iv = structure(list(var = c("N_CLI_PA_DDM_ACTUAL_MAX", "B_TARGET", "N_PA_ANTIG", "P_CLI_TC_DEUDA_v_CREDITO", "N_CLI_EDAD", "I_PA_PRODUCTO", "I_PA_AREA"),
                               vartype = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character"),
                               type = c("continuous", "categorical", "continuous", "continuous", "continuous", "categorical", "categorical"),
                               nobs = c(1000, 1000, 1000, 1000, 1000, 1000, 1000),
                               nlevels = c(14, 2, 13, 15, 16, 4, 3),
                               TotalWOE = c(-0.07966, 0.1408, 0.1407, -0.2057, 0.0634, 0.1914, -0.06667),
                               IV = c(0.02472, 0.004631, 0.004196, 0.00106, 0.0005945, 0.000401, 0.0002162),
                               IVAdj = c(0.02597, 0.01852, 0.004536, 0.001085, 0.0005945, 0.000802, 0.0005457)),
                          row.names = c(6L, 1L, 5L, 7L, 4L, 3L, 2L), class = "data.frame")
  expect_equal(dat.iv$IV, expected_iv)
})
#--------------------------------------- Continuous target ------------------------------------
