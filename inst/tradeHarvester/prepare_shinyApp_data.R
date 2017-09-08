
# Prepare data for a shiny app
#     The script is intended to prepare data for the shiny app deployed on shinyapps.io
# What is does:
#   Takes all WTO related data
#   Completes it with the missing values and saves in the separate files on the harddrive.

# Setup -------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(purrr)
library(readr)
# devtools::install_github("EBukin/tradeAnalysis-pack")
library(tradeAnalysis)

# Functions ---------------------------------------------------------------

#' Function for combining multiple data frames with mothly and annual CT data into one
#' and fill the gaps in annual data with aggregated monthly data or mirror data
#'
#' @param annualData,monthlyData data frames loaded with `load_ct_rdata()`.
#' @param filterHS,filterReporters atomic vectors with the list of reporters and partners
compl_ct_annual_data <-
  function(annualData,
           monthlyData,
           filterHS = NA,
           filterReporters = NA) {
    # filtering HS codes data
    annualData <- flt_com_ct(annualData, filterHS)
    monthlyData <- flt_com_ct(monthlyData, filterHS)
    
    # Generating data avilability
    dataAvailability <-
      get_combined_ct_avail(annualData, monthlyData, filterReporters)
    
    # Generating data tables with data for further join
    # All mirror data possible
    if (TRUE) {
      missingMirrorData <-
        dataAvailability  %>%
        filter(str_detect(Type, "Mir")) %>%
        mutate(Type = "Mirror") %>%
        select(-Reporter) %>%
        left_join(annualData, by = c("Reporter.Code", "Year", "Type"))
      if (nrow(missingMirrorData) == 0)
        missingMirrorData <- NULL
    } else {
      missingMirrorData <- NULL
    }
    
    # All montly data possible
    if (TRUE) {
      missingMonthlyAggData <-
        dataAvailability %>%
        filter(str_detect(Type, "Mon")) %>%
        mutate(Type = "Direct") %>%
        select(-Reporter) %>%
        left_join(monthlyData, by = c("Reporter.Code", "Year", "Type")) %>%
        agg_month_to_years() %>%
        mutate(Type = "Monthly")
      if (nrow(missingMonthlyAggData) == 0)
        missingMonthlyAggData <- NULL
    } else {
      missingMonthlyAggData <- NULL
    }
    
    # All available direct data
    availableDirectData <-
      dataAvailability  %>%
      filter(str_detect(Type, "Direct")) %>%
      select(-Reporter) %>%
      left_join(annualData, by = c("Reporter.Code", "Year", "Type"))
    
    # Join all data in order to fill the gaps
    gapFilledData <-
      bind_rows(missingMirrorData,
                missingMonthlyAggData,
                availableDirectData)
    gc()
    gapFilledData
  }

#' Local function for cleaning CT dataset used in the script below
clean_ct <-
  function(data) {
    data <-
      data %>%
      norm_ct_data() %>%
      filter(Variable == "Trade.Value..US..")
    data <-
      bind_rows(
        agg_commodities(data),
        filter(data, Commodity.Code %in% wtoAgFoodFull$Commodity.Code)
      ) %>%
      get_mirror_values()
    gc()
    data
  }

#' Local function for loading, cleaning and gapfilling all CT. Combines the
#' all aboveidentified functions for reading CT data from the storage,
#' cleaning it, mirroring values, combining gap filled dataset with the flags of values.
load_clean_ct <-
  function(x,
           rootFolderAnnual,
           rootFolderMonthly) {
    # Load annual CT data
    anDataCl <- load_ct_rdata(
      rootFolderAnnual,
      period = x,
      forceReload = TRUE,
      resave = F
    )
    
    if (nrow(anDataCl) > 0) {
      anDataCl <-
        anDataCl %>%
        clean_ct()
      
      if (x >= 2010) {
        mnData <- load_ct_rdata(
          rootFolderMonthly,
          period = x,
          forceReload = TRUE,
          resave = F
        ) %>%
          clean_ct()
      } else {
        mnData <-
          anDataCl[0,]
      }
      
      gapfData <-
        compl_ct_annual_data(anDataCl, mnData) %>%
        filter(!is.na(Commodity.Code))
    } else {
      gapfData <- tibble()
    }
    rm(mnData, anDataCl)
    gc()
    gapfData
  }


# Reloading all annual data reaggregating it ------------------------------

rootFolderAnnual = "~/ctData/ctBulkR/ctAnAll/wtoAnAll/"
rootFolderMonthly = "~/ctData/ctBulkR/ctMnAll/wtoMnAll/"

# Loading data
wtoAnAllAgData <-
  tibble(year = c(1990:2020)) %>%
  rowwise() %>%
  do({
    load_clean_ct(.$year, rootFolderAnnual, rootFolderMonthly)
  }) %>%
  ungroup()

# Saving data -------------------------------------------------------------

# All data as it is.
write_rds(wtoAnAllAgData,
          "~/ctData/ctBulkR/wtoAnAggAll.rds",
          compress = "gz")

# Short list of commodities
filterCommodities <- c(
  wtoAgFood$Commodity.Code,
  filter(wtoAnAllAgData, !Commodity.Code %in% wtoAgFoodFull$Commodity.Code)$Commodity.Code,
  filter(wtoAgFoodFull,cc2 != "", cc3 == "")$Commodity.Code,
  filter(wtoAgFoodFull,cc2 != "", cc3 != "", cc1 %in% c(paste0("0", 1:9), 10:17))$Commodity.Code
) %>% 
  unique()

write_rds(
  filter(wtoAnAllAgData, Commodity.Code %in% filterCommodities),
  "~/ctData/ctBulkR/wtoAnAggMain.rds",
  compress = "gz"
)

write_rds(
  filter(wtoAnAllAgData, !Commodity.Code %in% wtoAgFoodFull$Commodity.Code | Commodity.Code %in% wtoAgFood$Commodity.Code),
  "~/ctData/ctBulkR/wtoAnAggshort.rds",
  compress = "gz"
)


# Splitting data in separate files for Shiny app  -------------------------

# wtoAnAllAgData <- read_rds("~/ctData/ctBulkR/wtoAnAggAll.rds")

filter(wtoAnAllAgData, Commodity.Code %in% filterCommodities) %>%
  group_by(Reporter.Code) %>%
  do({
    x <- .
    code <- unique(x$Reporter.Code)
    write_rds(x, str_c("~/ctData/ShinyData/", code, ".rds"), compress = "gz")
    rm(x)
    gc()
    tibble()
  })

wtoAnAllAgData %>%
  sel_dist(Reporter.Code, Year, Type) %>%
  write_csv("~/ctData/ShinyData/dataAvailability.csv")
