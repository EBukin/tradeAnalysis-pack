# Eduard Bukin
# Loading data for GEO from the CT data file and saving it in one R_data object

# Setups ------------------------------------------------------------------

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
devtools::install_github("wilkox/treemapify")
library(treemapify)

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
  filter(Commodity.Code %in% wtoAgFood$Commodity.Code)

ctMonGeo <-
  ctMonGeo %>% 
  filter(Commodity.Code %in% wtoAgFoodFull$Commodity.Code)

# Analysis ----------------------------------------------------------------

# Define top n desetinations in each commodity, year
ctAnGeoTop <- 
  ctAnGeo %>% 
  rank_top_N_partners(5, otherEU = TRUE) %>% 
  mutate(Commodity.Code = as.factor(Commodity.Code),
         Partner.Code = as.factor(Partner.Code))



treeMapCoordinates <- 
  treemapify(
  ctAnGeoTop %>% filter(Year == 2014, Variable == "Trade.Value..US..", Trade.Flow.Code == 1, Partner.Code != 0),
  area = "Value",
  fill = "Value",
  label = "Partner.Code",
  group = "Commodity.Code"
)
# print(treeMapCoordinates)


ggplotify(treeMapCoordinates)




ctAnGeoTop %>% 
  filter(Trade.Flow.Code == 1, Year == 2010)


  aggTopNPartners(topN = 3, returnAggregates = FALSE) %>% View()



  group_by_(.dots = groupVars) %>%
  mutate(rank = min_rank(Value))

df <- ctAnGeo %>% 
  filter(Year == 2013, Commodity.Code == "01") 
