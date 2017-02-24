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
  filter(Commodity.Code %in% c(wtoAgFood$Commodity.Code, "TOTAL"),
         Variable == "Trade.Value..US..")

# Analysis ----------------------------------------------------------------


# Defining the most important component of trade --------------------------



# Calculating shares
shres_in_totals <-
  ctAnGeo %>% 
  agg_commodities() %>% 
  bind_rows(ctAnGeo %>% filter(Commodity.Code == "TOTAL")) %>% 
    calc_share_in("TOTAL") %>% 
    calc_share_in("WTO_AgriFood")

# Plotting first things
shres_in_totals %>% 
  filter(Commodity.Code %in% c("1_24_Total_excl_fish", "29_38_Chemicals", "29_53_AgriFoodGoods", "3_Fish", "51_Wool", "52_Cotton", "53_Fibers", "41_43_Skins", "50_Silk")) %>% 
  filter(Year %in% c(2010:2014),
         Partner.Code == 0,
         Trade.Flow.Code == 1) %>% 
  ggplot()+
  aes(x = Year, y = Share_in_WTO_AgriFood, colour = Commodity.Code, fill = Commodity.Code) +
  # geom_jitter()
  geom_bar(stat = "identity", position = "stack") 
  


# 1         1_24_AgriFood
# 2  1_24_Total_excl_fish
# 3       29_38_Chemicals
# 4   29_53_AgriFoodGoods
# 5                3_Fish
# 6               51_Wool
# 7             52_Cotton
# 8             53_Fibers
# 9          WTO_AgriFood
# 10          41_43_Skins
# 11              50_Silk
# 12                TOTAL


# Define top n desetinations in each commodity, year
agg <- 
  ctAnGeo %>% 
  agg_commodities() %>% 
  View()

  rank_top_N_partners(5, otherEU = TRUE) %>% 
  join_lables()
  

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
