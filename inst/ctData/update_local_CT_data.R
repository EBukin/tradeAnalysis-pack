# Script for downloading and/or updating trade data
#   The stodage for trade data is universal for amy apps and is buit locally

# Setup -------------------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(readr)
devtools::install_github("EBukin/tradeAnalysis-pack")
library(tradeAnalysis)

# Loading data ------------------------------------------------------------

# Available data:
ctAval <- getCTParameters()

# Loading token
token <- NA
token <- try(read_lines(".CT-token"), silent = TRUE)
if (class(try(read_lines(".CT-token"))) == "try-error") token <- NA

# Creating data structure with all folders
build_ct_storage()

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

# Initialising the reload folders structure
foldersStructure <-
  bind_rows(
    tibble(fromFolder = "~/ctData/ctBulkR/ctAnAll/",
           toFolder = "~/ctData/ctBulkR/ctAnAll/wtoAnAll/"),
    tibble(fromFolder = "~/ctData/ctBulkR/ctMnAll/",
           toFolder = "~/ctData/ctBulkR/ctMnAll/wtoMnAll/")
  )

# Reloading all annual data
# Loading every single R data file and saving its filtered version
foldersStructure %>%
  rowwise() %>%
  do({
    fromFolder = .$fromFolder
    toFolder = .$toFolder
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
  })


# Resaving all filtered data in one file
foldersStructure %>%
  rowwise() %>%
  do({
    wto_an <-
      load_some_ct_bulks_rdata(.$toFolder,
                               filesList = listCTdata(.$toFolder, "Rdata")$name)
    readr::write_rds(wto_an,
                     path = file.path(.$toFolder, "AllInOne.rds"),
                     compress = "gz")
    rm(wto_an)
    gc()
    tibble()
  })

