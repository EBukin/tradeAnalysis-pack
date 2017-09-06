# Script for downloading and/or updating trade data
#   The stodage for trade data is universal for amy apps and is buit locally

# Setup -------------------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(readr)
devtools::install_github("EBukin/tradeAnalysis", ref = "pack")
library(tradeAnalysis)

# Loading data ------------------------------------------------------------

# Available data:
ctAval <- getCTParameters()

# Loading token
token <- NA
token <- read_lines(".CT-token")
if (token == "NA") token <- NA

# Creating data structure with all folders
build_ct_storage_structure()

# Data in the folders ----------------------------------------------------

# Monthly all countries
updateCTData(
  ctCurData = ctAval,
  toFolder = "~/ctData/ctBulkZIP/ctMnAll/",
  frequency = "MONTHLY",
  reporters = "ALL",
  token = token
)

gc()

# Resaving CT datafiles to R data files
resave_ct_to_Rdata_all(fromFolder =  "~/ctData/ctBulkZIP/ctMnAll/",
                       toFolder = "~/ctData/ctBulkR/ctMnAll/")

gc()

# # Annual all countries
updateCTData(
  ctCurData = ctAval,
  toFolder = "~/ctData/ctBulkZIP/ctAnAll/",
  frequency = "ANNUAL",
  reporters = "ALL",
  token = token
)

gc()

# Resaving CT datafiles to R data files
resave_ct_to_Rdata_all(fromFolder =  "~/ctData/ctBulkZIP/ctAnAll/",
                       toFolder = "~/ctData/ctBulkR/ctAnAll/")

gc()

# # Mounthly by country
# updateCTData(ctCurData = ctAval,
#              toFolder = "../data/data_raw/ct_zip/m_by_country/",
#              frequency = "MONTHLY",
#              reporters = getCTReporters()$Reporter.Code,
#              token = token)
#
# # Annual by country
# updateCTData(ctCurData = ctAval,
#              toFolder = "../data/data_raw/ct_zip/a_by_country/",
#              frequency = "ANNUAL",
#              reporters = getCTReporters()$Reporter.Code,
#              token = token)

# Filtering and reloading only WTO relevant data --------------------------

# Reloading all annual data
# Loading every single R data file and saving its filtered version
fromFolder = "~/ctData/ctBulkR/ctAnAll/"
toFolder = "~/ctData/ctBulkR/ctAnAll/wtoAnAll/"

flt_data_to_reload(fromFolder, toFolder, newPrefix = "wto-") %>%
  rowwise() %>%
  do({
    if (!is.na(.$destOldFile)) {
      moveToOld(.$destOldFile, toFolder)
    }
    assign(
      .$destNewFile,
      load_some_ct_bulks_rdata(
        filesList = .$originNewFile,
        fromFolder = fromFolder,
        hsCodes = wtoAgFoodFull$Commodity.Code
      )
    )
    save(list = .$destNewFile,
         file = file.path(toFolder, .$destNewFile))
    rm(list = .$destNewFile)
    gc()
    tibble()
  })

# Resaving all filtered data in one file
# wto_an <-
#   load_some_ct_bulks_rdata(fromFolder,
#                            filesList = listCTdata(fromFolder, "Rdata")$name)
# 
# readr::write_rds(wto_an,
#                  path = file.path(toFolder, "AllInOne"),
#                  compress = "gz")
# rm(wto_an)
# gc()

# Reloading all monthly data -----------------------------------------------

fromFolder = "~/ctData/ctBulkR/ctMnAll/"
toFolder = "~/ctData/ctBulkR/ctMnAll/wtoMnAll/"

flt_data_to_reload(fromFolder, toFolder, newPrefix = "wto-") %>%
  rowwise() %>%
  do({
    if (!is.na(.$destOldFile)) {
      moveToOld(.$destOldFile, toFolder)
    }
    assign(
      .$destNewFile,
      load_some_ct_bulks_rdata(
        filesList = .$originNewFile,
        fromFolder = fromFolder,
        hsCodes = wtoAgFoodFull$Commodity.Code
      )
    )
    save(list = .$destNewFile,
         file = file.path(toFolder, .$destNewFile))
    rm(list = .$destNewFile)
    gc()
    tibble()
  })

# wto_mn <-
#   load_some_ct_bulks_rdata(fromFolder,
#                            filesList = listCTdata(fromFolder, "Rdata")$name)
# 
# readr::write_rds(wto_mn,
#                  path = file.path(toFolder, "AllInOne"),
#                  compress = "gz")
# rm(wto_mn)
# gc()
