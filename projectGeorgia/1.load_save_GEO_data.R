# Eduard Bukin
# Loading data for GEO from the CT data file and saving it in one R_data object

# Setups ------------------------------------------------------------------

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Loading funcitons
l_ply(str_c("R/", list.files("R/",pattern="*.R")), source)

# Setuping parallel connection --------------------------------------------

# library(doSNOW)

# Reading GEO data ------------------------------------------------------------

# Georgian Area Code
geo <- "268"

# Loading Annual data
anFolder <- "../data/data_raw/ct_zip/a_by_country/"
allAnDataFiles <- listCTdata(anFolder)

# Reading data
ctAnGeo <-
  ddply(
    .data =
      allAnDataFiles %>% 
      filter(r == geo),
    .variables = .(name),
    .fun = function(x) {
      readCTZIP(file = x$name, folder = anFolder)
    },
    .parallel = FALSE,
    .progress = "text"
  ) %>%
  tbl_df() %>%
  select(-name) %>% 
  norm_ct_data()

# Loading monthly data
monthFolder <- "../data/data_raw/ct_zip/m_by_country/"
allMonDataFiles <- listCTdata(monthFolder)

# Reading data
ctMonGeo <-
  ddply(
    .data =
      allMonDataFiles %>% 
      filter(r == geo),
    .variables = .(name),
    .fun = function(x) {
      readCTZIP(file = x$name, folder = monthFolder)
    },
    .parallel = FALSE,
    .progress = "text"
  ) %>%
  tbl_df() %>%
  select(-name) %>% 
  norm_ct_data()

# Saving data -------------------------------------------------------------

save(ctAnGeo, ctMonGeo, file = "projectGeorgia/ctGEO.Rdata")


