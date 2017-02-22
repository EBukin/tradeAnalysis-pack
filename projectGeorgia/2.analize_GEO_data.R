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

# Loading data ------------------------------------------------------------

load("projectGeorgia/ctGEO.Rdata")
load("data/wtoAgFood.Rdata")
load("data/sampleData.Rdata")

# Filtering commodities which we need only


# Analysis ----------------------------------------------------------------

# Creating sample test data
ctSample <- 
  ctAnGeo %>% 
  filter(Year >= 2010, Commodity.Code %in% wtoAgFood$Commodity.Code) 
save(ctSample, file ="data/sampleData.Rdata")

# Define top n desetinations in each commodity, year
ctAnGeoTop <- 
  ctAnGeo %>% 
  filter(Trade.Flow.Code == 1, Year == 2010, Commodity.Code == "02") %>% 
  rank_top_N_partners(5, otherEU = TRUE) 


ctAnGeoTop %>% 
  filter(Trade.Flow.Code == 1, Year == 2010)


  aggTopNPartners(topN = 3, returnAggregates = FALSE) %>% View()



  group_by_(.dots = groupVars) %>%
  mutate(rank = min_rank(Value))

df <- ctAnGeo %>% 
  filter(Year == 2013, Commodity.Code == "01") 
