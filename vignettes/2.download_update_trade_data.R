# Script for downloading and/or updating trade data


# Setup -------------------------------------------------------------------

library(data.table)
library(dplyr)
library(tidyr)
library(jsonlite)
library(stringr)

# Loading funcitons
l_ply(str_c("R/", list.files("R/", pattern="*.R")), source)

# Loading data ------------------------------------------------------------

# Reporters
ctRep <- getCTReporters()

# Partners
ctPart <- getCTPartners()

# # Available data:
ctAval <- jsonlite::fromJSON("http://comtrade.un.org/api/refs/da/bulk?parameters") %>% tbl_df()

# Data in the folders ----------------------------------------------------

# # Monthly all countries
updateCTData(ctAval,
             folder = "../data/data_raw/ct_zip/m_all/",
             period = "MONTHLY",
             repo = "ALL" )

# Mounthly by country
updateCTData(ctAval,
             folder = "../data/data_raw/ct_zip/m_by_country/",
             period = "MONTHLY",
             repo = getCTReporters()$Reporter.Code )

# # Annual all countries
updateCTData(ctAval,
             folder = "../data/data_raw/ct_zip/a_all/",
             period = "ANNUAL",
             repo = "ALL")

# Annual by country
updateCTData(ctAval, 
             folder = "../data/data_raw/ct_zip/a_by_country/", 
             period = "ANNUAL", 
             repo = getCTReporters()$Reporter.Code)



