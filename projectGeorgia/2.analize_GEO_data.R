# Eduard Bukin
# Loading data for GEO from the CT data file and saving it in one R_data object

# Setups ------------------------------------------------------------------

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
devtools::install_github("ricardo-bion/ggtech", 
                         dependencies=TRUE)
library(ggtech)
# devtools::install_github("wilkox/treemapify")
# library(treemapify)

# Loading funcitons
l_ply(str_c("R/", list.files("R/",pattern="*.R")), source)

# Setuping parallel connection --------------------------------------------

# library(doSNOW)

# Loading data ------------------------------------------------------------

load("projectGeorgia/ctGEO.Rdata")
load("data/wtoAgFood.Rdata")
load("data/sampleData.Rdata")

# Creating sample test data
# ctSample <- 
#   ctAnGeo %>% 
#   filter(Year >= 2010, Commodity.Code %in% wtoAgFood$Commodity.Code) 
# save(ctSample, file ="data/sampleData.Rdata")

# Filtering commodities which we need only
ctAnGeo <-
  ctAnGeo %>% 
  filter(Commodity.Code %in% c(wtoAgFood$Commodity.Code, "TOTAL"),
         Variable == "Trade.Value..US..")%>% 
  agg_commodities() %>% 
  bind_rows(ctAnGeo %>% filter(Commodity.Code == "TOTAL"))

ctMonGeo <-
  ctMonGeo %>% 
  filter(Commodity.Code %in% c(wtoAgFood$Commodity.Code, "TOTAL"),
         Variable == "Trade.Value..US..") %>% 
  agg_commodities() %>% 
  bind_rows(ctAnGeo %>% filter(Commodity.Code == "TOTAL"))

# Analysis ----------------------------------------------------------------


# Defining the most important component of trade --------------------------

ctMonGeo %>% 
  filter(Commodity.Code %in% c("TOTAL")) %>% 
  filter(Year %in% c(2010:2014)) %>% 
  rank_agg_top_partners(8) %>% 
  filter(Partner.Code != 0) %>% 
  plot_tb(stackVar = "Partner", manualScale = T, brewPalName = "Set2")


# Calculating shares
shres_in_totals <-
  ctAnGeo %>% 
    calc_share_in("TOTAL") %>% 
    calc_share_in("WTO_AgriFood")

