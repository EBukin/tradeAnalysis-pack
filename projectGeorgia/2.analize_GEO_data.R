# Eduard Bukin
# Loading data for GEO from the CT data file and saving it in one R_data object

# Setups ------------------------------------------------------------------

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(plotly)

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
ctAnGeoAg <-
  ctAnGeo %>% 
  filter(Commodity.Code %in% c(wtoAgFood$Commodity.Code, "TOTAL"),
         Variable == "Trade.Value..US..")%>% 
  agg_commodities() %>% 
  bind_rows(ctAnGeo %>% filter(Commodity.Code == "TOTAL")) %>% 
  mutate(Value = Value / 1000000)

ctMonGeoAg <-
  ctMonGeo %>% 
  filter(Commodity.Code %in% c(wtoAgFood$Commodity.Code, "TOTAL"),
         Variable == "Trade.Value..US..") %>% 
  agg_commodities() %>% 
  bind_rows(ctMonGeo %>% filter(Commodity.Code == "TOTAL"))%>% 
  mutate(Value = Value / 1000000)

# Analysis ----------------------------------------------------------------


# Defining the most traded partners in last years --------------------------

totalTrade <-
  ctAnGeoAg %>% 
  filter(Commodity.Code %in% c("TOTAL")) %>% 
  filter(Year %in% c(2011:2016))%>% 
  filter(Partner.Code != 0)

# All trade Import
totalTrade %>% 
  rank_agg_top_partners(10, oneEU = FALSE, otherEU = TRUE, oneFSR = FALSE)  %>% 
  filter(Trade.Flow.Code == 1) %>% 
  plot_tb(stackVar = "Partner", brewScale = T, brewPalName = "Set2", plotTradeBalance = FALSE) %>% 
  ggplotly(p =.)

# All trade Export
totalTrade %>% 
  rank_agg_top_partners(10, oneEU = FALSE, otherEU = TRUE, oneFSR = FALSE) %>% 
  filter(Trade.Flow.Code == 2) %>% 
  plot_tb(stackVar = "Partner", brewScale = T, brewPalName = "Set2", plotTradeBalance = FALSE) %>% 
  ggplotly(p =.)

# All commodities trade balance
totalTrade %>% 
  rank_agg_top_partners(5, oneEU = FALSE, otherEU = TRUE, oneFSR = FALSE) %>%  
  plot_tb(stackVar = "Partner", brewScale = T, brewPalName = "Set2") 


# Analizing main trade partners for WTO commodities -----------------------

wtoTrade <-
  ctAnGeoAg %>% 
  filter(Commodity.Code %in% c("WTO_AgriFood")) %>% 
  filter(Year %in% c(2011:2016))%>% 
  filter(Partner.Code != 0)

# Import
wtoTrade %>% 
  rank_agg_top_partners(10, oneEU = FALSE, otherEU = TRUE, oneFSR = FALSE) %>% 
  filter(Trade.Flow.Code == 1) %>% 
  plot_tb(stackVar = "Partner", brewScale = T, brewPalName = "Set2", plotTradeBalance = FALSE) %>% 
  ggplotly(p =.)

# export
wtoTrade %>% 
  rank_agg_top_partners(10, oneEU = FALSE, otherEU = TRUE, oneFSR = FALSE) %>% 
  filter(Trade.Flow.Code == 2) %>% 
  plot_tb(stackVar = "Partner", brewScale = T, brewPalName = "Set2", plotTradeBalance = FALSE) %>% 
  ggplotly(p =.)

# Import
wtoTrade %>% 
  rank_agg_top_partners(5, oneEU = FALSE, otherEU = TRUE, oneFSR = FALSE) %>% 
  plot_tb(stackVar = "Partner", brewScale = T, brewPalName = "Set2") 


# Structure of WTO trade by commodity -------------------------------------

wtoTradeStr <-
  ctAnGeoAg %>% 
  filter(!Commodity.Code %in% c("TOTAL", "WTO_AgriFood", "29_53_AgriFoodGoods", "1_24_AgriFood")) %>% 
  filter(Year %in% c(2011:2016))%>% 
  filter(Partner.Code == 0)

# import
wtoTradeStr%>% 
  filter(Trade.Flow.Code == 1) %>% 
  plot_tb(stackVar = "Commodity.Code", brewScale = T, brewPalName = "Set2", plotTradeBalance = FALSE) %>% 
  ggplotly(p =.)

# export
wtoTradeStr%>% 
  filter(Trade.Flow.Code == 2) %>% 
  plot_tb(stackVar = "Commodity.Code", brewScale = T, brewPalName = "Set2", plotTradeBalance = FALSE) %>% 
  ggplotly(p =.)

# Trade balance
wtoTradeStr%>% 
  plot_tb(stackVar = "Commodity.Code", brewScale = T, brewPalName = "Set2") 


# Strucutre Ag trade by commodity -----------------------------------------


agTrade <-
  ctAnGeo %>% 
  filter(Commodity.Code %in% c(wtoAgFood$Commodity.Code, "TOTAL"),
         Variable == "Trade.Value..US..",
         Year %in% c(2010:2016))%>% 
  agg_commodities(.,onlyAggregates = FALSE) %>% 
  mutate(Value = Value / 1000000)

AgriFood <- unique(agTrade$Commodity.Code)[1:24]

# TradeBalance by comodity
agTrade %>% 
  filter(Commodity.Code %in% AgriFood,
         Partner.Code == 0) %>% 
  plot_tb(stackVar = "Commodity", brewScale = T, brewPalName = "Set2") 

# Export by comodity
agTrade %>% 
  filter(Commodity.Code %in% AgriFood,
         Trade.Flow.Code == 2,
         Partner.Code == 0) %>% 
  plot_tb(stackVar = "Commodity", brewScale = T, brewPalName = "Set2", plotTradeBalance = F) %>% 
  ggplotly()
  
  


  
p %>% 
  filter(Trade.Flow == "Import") %>% 
  # droplevels() %>% 
  ggvis(x = ~Period, y = ~Value, fill = ~Partner) %>% 
  layer_bars()
  
p %>% 
  # filter(Trade.Flow == "Import") %>% 
  filter(Value !=0) %>% 
  plot_ly(., type="bar", x = ~Period, y = ~Value, color = ~Partner) %>% 
  # add_trace(y = ~Value, name = 'LA Zoo') %>%
  layout(barmode = 'stack')




pp <- 
  ctAnGeoAg %>% 
  filter(Commodity.Code %in% c("TOTAL")) %>% 
  filter(Year %in% c(2012:2014)) %>% 
  rank_agg_top_partners(5, oneEU = FALSE, otherEU = TRUE, oneFSR = FALSE) %>% 
  filter(Partner.Code != 0) %>% 
  plot_tb(stackVar = "Partner", brewScale = T, brewPalName = "Set1", returnData = F) 

pp
gg2list(pp)


set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = ~carat, y = ~price, color = ~carat,
        size = ~carat, text = ~paste("Clarity: ", clarity))


# 
# +
#   scale_fill_brewer(palette = "Pastel1", direction = -1)
#   scale_fill_manual(values = rev(hue_pal(h = c(0, 360) , c = 100, l = 60, h.start = 30, direction = 1)(9)))


# devtools::install_github("ndphillips/yarrr")
# library("yarrr")

piratepal(palette = "all", action = "show")

hue_pal(h = c(0, 360) , c = 80, l = 70, h.start = 25, direction = 1)(10)

# Calculating shares
shres_in_totals <-
  ctAnGeoAg %>% 
    calc_share_in("TOTAL") %>% 
    calc_share_in("WTO_AgriFood")

