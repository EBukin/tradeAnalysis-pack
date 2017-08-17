# Prepare data for shiny app
#     The script is intended to prepare data for the shiny app.



library(tidyverse)
library(stringr)
library(tradeAnalysis)
library(purrr)


# Functions ---------------------------------------------------------------

# Function for combining multiple data objects into one
compl_ct_annual_data <-
  function(annualData,
           monthlyData,
           filterHS = NA,
           filterReporters = NA) {
    annualData <- flt_com_ct(annualData, filterHS)
    monthlyData <- flt_com_ct(monthlyData, filterHS)
    dataAvailability <-
      get_combined_ct_avail(annualData, monthlyData, filterReporters)
    
    
    if (TRUE) {
      MissingMirrorData <-
        dataAvailability  %>%
        filter(str_detect(Type, "Mir")) %>%
        mutate(Type = "Mirror") %>%
        select(-Reporter) %>%
        left_join(annualData, by = c("Reporter.Code", "Year", "Type"))
      if (nrow(MissingMirrorData) == 0)
        MissingMirrorData <- NULL
    } else {
      MissingMirrorData <- NULL
    }
    if (TRUE) {
      MissingMonthlyAggData <-
        dataAvailability %>%
        filter(str_detect(Type, "Mon")) %>%
        mutate(Type = "Direct") %>%
        select(-Reporter) %>%
        left_join(monthlyData, by = c("Reporter.Code", "Year", "Type")) %>%
        agg_month_to_years() %>%
        mutate(Type = "Monthly")
      if (nrow(MissingMonthlyAggData) == 0)
        MissingMonthlyAggData <- NULL
    } else {
      MissingMonthlyAggData <- NULL
    }
    #  Available direct annual data
    AvailableDirectData <-
      dataAvailability  %>%
      filter(str_detect(Type, "Direct")) %>%
      select(-Reporter) %>%
      left_join(annualData, by = c("Reporter.Code", "Year", "Type"))
    gapFilledData <-
      bind_rows(MissingMirrorData,
                MissingMonthlyAggData,
                AvailableDirectData)
    gc()
    gapFilledData
  }


# Reloading all annual data reaggregating it ------------------------------
rootFolderAnnual = "~/ctData/ctBulkR/ctAnAll/wtoAnAll/"
rootFolderMonthly = "~/ctData/ctBulkR/ctMnAll/wtoMnAll/"

x <- 2000

clean_ct <- function(dt) {
  dt <-
    dt %>%
    norm_ct_data() %>%
    filter(Variable == "Trade.Value..US..")
  
  dt <-
    bind_rows(agg_commodities(dt),
              filter(dt, Commodity.Code %in% wtoAgFood$Commodity.Code)) %>%
    get_mirror_values()
  gc()
  dt
}

load_clean_ct <- function(x, rootFolderAnnual, rootFolderMonthly) {
  anData <- load_ct_rdata(
    rootFolderAnnual,
    period = x,
    forceReload = TRUE,
    resave = F
  )
  anDataCl <-
    anData %>%
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
      anData[0,] %>%
      clean_ct()
  }
  
  gapfData <-
    compl_ct_annual_data(anDataCl, mnData) %>%
    filter(!is.na(Commodity.Code))
  rm(anData, mnData, anDataCl)
  gc()
  gapfData
}


# Loading data ------------------------------------------------------------

wtoAnAllAgData <-
  tibble(year = c(1992:2016)) %>%
  rowwise() %>%
  do({
    load_clean_ct(.$year, rootFolderAnnual, rootFolderMonthly)
  }) %>%
  ungroup()

# Saving data -------------------------------------------------------------

readr::write_rds(wtoAnAllAgData, "~/ctData/ctBulkR/wtaAnAggAll.rds", compress = "gz")
readr::write_rds(filter(wtoAnAllAgData, !Commodity.Code %in% wtoAgFood$Commodity.Code), "~/ctData/ctBulkR/wtaAnAggShort.rds", compress = "gz")
