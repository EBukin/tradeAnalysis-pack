# Analyzing trade potential by regions


# Setups ------------------------------------------------------------------

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidyverse)

source("R/grid_arrange_shared_legend.R")

# Loading data ------------------------------------------------------------

file <- "globalTrade/baseline2016.csv"

# Outlook data
outlook <-
  read.csv(file, header = FALSE, stringsAsFactors = FALSE) %>%
  tbl_df()

names(outlook) <- slice(outlook, 2)

ol <-
  outlook %>%
  select(-`NA`) %>%
  slice(3:nrow(.)) %>%
  select(-Location, -Type, -Comment) %>%
  separate(`OUTPUT,0`, c("AreaCode", "ItemCode", "ElementCode"), sep = "_") %>%
  gather(Year, Value, 4:length(.)) %>%
  mutate(Year = as.integer(str_sub(Year, 1, 4)),
         Value = as.numeric(Value))

# Nutrients data
nutr <-
  read.csv("globalTrade/nutcoef.csv", header = FALSE, stringsAsFactors = FALSE) %>%
  tbl_df()

names(nutr) <- slice(nutr, 2)

nutr <-
  nutr %>%
  select(-`NA`) %>%
  slice(3:nrow(.)) %>%
  select(-Location, -Type, -Comment) %>%
  separate(`OUTPUT,0`, c("AreaCode", "ItemCode", "ElementCode"), sep = "_") %>%
  gather(Year, Value, 4:length(.)) %>%
  mutate(Year = as.integer(str_sub(Year, 1, 4)),
         Value = as.numeric(Value))
# Nutrients are measurted in:
# calCont = kcal/kg
# FatCont = fat content %
# ProtCont = protein content %



# Initializing mapping tables ---------------------------------------------

World <-
  c(
    "USA",
    "ARG",
    "AUS",
    "BRA",
    "CAN",
    "CHE",
    "CHN",
    "E15",
    "EUN",
    "JPN",
    "KOR",
    "MEX",
    "NMS",
    "NOR",
    "NZL",
    "RUS",
    "AFL",
    "AFN",
    "AFS",
    "ASA",
    "ASD",
    "ASL",
    "BGD",
    "CHL",
    "COL",
    "DZA",
    "EGY",
    "ETH",
    "EUE",
    "EUW",
    "GHA",
    "HTI",
    "IDN",
    "IND",
    "IRN",
    "ISR",
    "KAZ",
    "MLE",
    "MOZ",
    "MYS",
    "NGA",
    "OCE",
    "OCL",
    "PAK",
    "PER",
    "PHL",
    "PRY",
    "SAC",
    "SAU",
    "SDN",
    "THA",
    "TUR",
    "TZA",
    "UKR",
    "URY",
    "VNM",
    "ZAF",
    "ZMB"
  )

Africa <-
  c("AFL","AFN","AFS","DZA","EGY","ETH","GHA","MOZ","NGA","SDN","TZA","ZAF","ZMB")

Asia <-
  c("CHN","JPN","KOR","ASA","ASL","BGD","IDN","IND","IRN","MLE",
    "MYS","PAK","PHL","SAU","THA","VNM","ISR","KAZ")

Oceania <-
  c("AUS", "NZL", "OCE", "OCL")

AsiaOceania <-
  c("CHN","JPN","KOR","ASA","ASD","ASL","BGD","IDN","IND","IRN","MLE",
    "MYS","PAK","PHL","SAU","THA","TUR","VNM","ISR", "AUS", "NZL", "OCE", "OCL")

Europe <-
  c("CHE", "EUN", "NOR", "RUS", "EUE", "EUW", "UKR", "TUR", "ASD", "KAZ")
# EU is excluded ,"E15", "NMS"

NorthAmerica <-
  c("USA", "CAN")

SouthAmerica <-
  c("ARG","BRA","MEX","CHL","COL","PER","PRY","SAC","URY","HTI") # ok

# Initializing regions mapping table
regionsMT <-
  bind_rows(
    tibble(Region = "Africa", AreaCode = Africa),
    #tibble(Region = "Asia", AreaCode = Asia),
    #tibble(Region = "Oceania", AreaCode = Oceania),
    tibble(Region = "Asia Oceania", AreaCode = AsiaOceania),
    tibble(Region = "Europe", AreaCode = Europe),
    tibble(Region = "North America", AreaCode = NorthAmerica),
    tibble(Region = "Latin America and Caribbeans America", AreaCode = SouthAmerica),
    tibble(Region = "World", AreaCode = c(Africa, AsiaOceania, Europe, NorthAmerica, SouthAmerica))
  )

# Year mapping table
yearsMT <-
  tibble(YearAggregate = sort(rep(str_c(
    seq(2001, 2021, 5), seq(2005, 2025, 5), sep = "-"
  ), 5)), Year = 2001:2025)

# Commodities
Wheat <- "WT"
Rice <- "RI"
Maize <- "MA"
CoarsGrains <- c("OCG")
Oilseeds <- c("SB", "OOS")
Meat <- c("BV", "PT", "PK", "SH")
Sugar <- "SU"
VegetableOils <- c("PL", "KL", "CL", "CSE", "VLL", "PLL")
PalmOils <- c("KM", "CM")

itemsMT <-
  bind_rows(
    tibble(
      type = "Grains",
      Item = "Wheat",
      ItemCode = Wheat
    ),
    tibble(
      type = "Rice",
      Item = "Rice",
      ItemCode = Rice
    ),
    tibble(
      type = "Grains",
      Item = "Maize",
      ItemCode = Maize
    ),
    tibble(
      type = "Grains",
      Item = "Coars grains",
      ItemCode = CoarsGrains
    ),
    tibble(
      type = "Oilseeds",
      Item = "Soybeans", 
      ItemCode = Oilseeds
    ),
    tibble(
      type = "Meat",
      Item = c("Beef and veal", "Poultry", "Pork", "Sheep"),
      ItemCode = Meat
    ),
    tibble(
      type = "Sugar",
      Item = "Sugar",
      ItemCode = Sugar
    ),
    tibble(
      type = "VegetableOils",
      Item = c("PL", "KL", "CL", "CSE", "Vegetable oils", "Palm oils"),
      ItemCode = VegetableOils
    ),
    tibble(
      type = "PalmOils",
      Item = c("KM", "CM"),
      ItemCode = PalmOils
    )
  )

# Flows
Flows <- c("QP", "QC", "IM", "EX", "ST", "FO", "OU", "FE")

# Nutrient specific items 
nutrients <- c(Wheat, Rice, Maize, CoarsGrains, Oilseeds, Meat, Sugar)


# Analyzing nutrients for regions -----------------------------------------------------

# # Generic non aggregated nutrients data
# nutrData <-
#   ol %>%
#   filter(AreaCode %in% regionsMT$AreaCode,
#          ItemCode %in% nutrients,
#          ElementCode %in% Flows) %>% 
#   rename(ValueQuantity = Value) %>% 
#   left_join(nutr, by = c( "AreaCode", "ItemCode", "Year")) %>%
#   mutate(ValueNutrient = ValueQuantity *  Value) %>% 
# # Nutrients are measurted in:
# # calCont = kcal/kg
# # FatCont = fat content %
# # ProtCont = protein content %
#   mutate(ValueNutrient = ifelse(ElementCode.y == "calCont", ValueNutrient / 1000, ValueNutrient),
#          ElementCode.y = ifelse(ElementCode.y == "calCont", "Calories, billion calories", ElementCode.y),
#          ValueNutrient = ifelse(ElementCode.y == "FatCont", ValueNutrient / 100, ValueNutrient),
#          ElementCode.y = ifelse(ElementCode.y == "FatCont", "Fat content, 1000 tones", ElementCode.y),
#          ValueNutrient = ifelse(ElementCode.y == "ProtCont", ValueNutrient / 100, ValueNutrient),
#          ElementCode.y = ifelse(ElementCode.y == "ProtCont", "Proteins content, 1000 tones", ElementCode.y))
#   
#   
# # Plotting nutriens for one country
# nutr_Test <-
#   nutrData %>% 
#   left_join(regionsMT, by = "AreaCode") %>%
#   group_by(Region, ItemCode, ElementCode.x, ElementCode.y,  Year) %>%
#   summarise(Value = sum(ValueNutrient, na.rm = TRUE)) %>%
#   
#   # Making average yearstotla calories content
#   left_join(yearsMT, by = "Year") %>%
#   group_by(Region, ElementCode.x, ElementCode.y,  YearAggregate) %>%
#   summarise(Value = sum(Value, na.rm = TRUE)/ 5)  %>% 
#   filter(!is.na(YearAggregate)) %>% 
#   ungroup() %>%
#  filter(ElementCode.x %in% c("QC", "QP"))
# 
# 
# 
# # Plotting one with all plots
# units <- unique(nutr_Test$ElementCode.y)
# titleBig <- "Chage from 2001 to 2025"
# p <- nutr_Test %>%
#   spread(ElementCode.x, Value) %>%
#   ggplot(.,
#          aes(
#            QC,
#            QP,
#            group = Region,
#            colour = Region,
#            fill = Region
#          )) +
#   # geom_label(mapping = aes(label = Lable), vjust = "inward", hjust = "inward")+
#   geom_point() +
#   geom_line(arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
#   geom_abline(mapping = aes(intercept = 0, slope = 1))  +
#   theme_bw() +
#   # scale_y_log10()+
#   # scale_x_log10()+
#   # expand_limits(x = c(0, max(nutr_Test$Value)), y = c(0, max(nutr_Test$Value))) +
#   labs(title = titleBig,
#        x = "Consumption",
#        y = "Production") +
#   scale_color_brewer(type = "qual", palette = 2) +
#   # coord_fixed(xlim =  c(0, max(nutr_Test$Value)), ylim = c(0, max(nutr_Test$Value)))+
#   coord_fixed() +
#   facet_wrap( ~ ElementCode.y,
#               nrow = 2,
#               ncol = 2,
#               scales = "free") 
# 
# ggsave(str_c(
#       "globalTrade/output/Nutrients_change_all_nutrients.png"
#     ),
#     plot = p,
#     dpi = 600
#   )
# 
# # Mass plotting
# d_ply(nutr_Test, .(ElementCode.y), function(nutr_Test2){
#   units <- unique(nutr_Test2$ElementCode.y)
#   titleBig <- "Chage from 2001 to 2025"
#   p <- nutr_Test2 %>%
#     spread(ElementCode.x, Value) %>%
#     ggplot(.,
#            aes(
#              QC,
#              QP,
#              group = Region,
#              colour = Region,
#              fill = Region
#            )) +
#     # geom_label(mapping = aes(label = Lable), vjust = "inward", hjust = "inward")+
#     geom_point() +
#     geom_line(arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
#     geom_abline(mapping = aes(intercept = 0, slope = 1))  +
#     theme_bw() +
#     # scale_y_log10()+
#     # scale_x_log10()+
#     # expand_limits(x = c(0, max(nutr_Test$Value)), y = c(0, max(nutr_Test$Value))) +
#     labs(title = units,
#          x = "Consumption",
#          y = "Production") +
#     scale_color_brewer(type = "qual", palette = 2) +
#     coord_fixed(xlim =  c(0, max(nutr_Test2$Value)), ylim = c(0, max(nutr_Test2$Value)))
#   
#   ggsave(str_c(
#     "globalTrade/output/Nutrients_change_",
#     unique(nutr_Test2$ElementCode.y),
#     ".png"
#   ),
#   plot = p,
#   dpi = 600
#   )
# }, .progress = "text")


# Looking at the net trade ------------------------------------------------

# nutrTSDate <- 
#   nutrData %>% 
#   filter(ElementCode.x %in% c("EX", "IM")) %>% 
#   
#   # Aggregating regions
#   left_join(regionsMT, by = "AreaCode") %>%
#   group_by(Region, ItemCode, ElementCode.x, ElementCode.y,  Year) %>%
#   summarise(Value = sum(ValueNutrient, na.rm = TRUE)) %>%
#   group_by(Region, ElementCode.x, ElementCode.y,  Year) %>%
#   summarise(Value = sum(Value, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(Value = ifelse(ElementCode.x == "IM", -Value , Value))
# 
# # Plotting facet
# ptb <- 
#   nutrTSDate %>% 
#   # filter(ElementCode.y == "Calories, billion calories") %>% 
#   ggplot(., aes(Year, Value, fill = Region)) +
#   geom_hline(aes(yintercept = 0)) +
#   geom_bar(
#     colour = "black",
#     stat = "identity",
#     position = "stack") +
#   labs(title = "Global trade of nutrients",
#        y = "-Import / +Export") + 
#   facet_wrap(~ ElementCode.y, ncol = 1, scales = "free") +
#   scale_fill_brewer(type = "qual", palette = 7)
# ptb
# 
# ggsave(str_c(
#   "globalTrade/output/Nutrients_trade_balance_by_regions",
#   # unique(nutrTSDate$ElementCode.y),
#   ".png"
# ),
# plot = ptb,
# dpi = 600
# )
# 
# # Plotting all
# d_ply(nutrTSDate, .(ElementCode.y), function(nutrTSDate2){
#   ptb <- 
#     nutrTSDate2 %>% 
#     # filter(ElementCode.y == "Calories, billion calories") %>% 
#     ggplot(., aes(Year, Value, fill = Region)) +
#     geom_hline(aes(yintercept = 0)) +
#     geom_bar(
#       colour = "black",
#       stat = "identity",
#       position = "stack") +
#     labs(title = "Global trade of nutrients",
#          subtitle = unique(nutrTSDate2$ElementCode.y),
#          y = "-Import / +Export") +
#     scale_fill_brewer(type = "qual", palette = 7)
#   # ptb
#   
#   ggsave(str_c(
#     "globalTrade/output/Nutrients_trade_balance_by_regions_",
#     unique(nutrTSDate2$ElementCode.y),
#     ".png"
#   ),
#   plot = ptb,
#   dpi = 600
#   )
# }, .progress = "text")

# Analysis of Conumption vs production -------------------------------------

# Making data for calculaintg country specific changes
pRegData_FULL <-
  ol %>%
  filter(AreaCode %in% regionsMT$AreaCode,
         # ItemCode %in% nutrients,
         ElementCode %in% Flows) %>%
  
  # Agg regions
  left_join(regionsMT, by = "AreaCode") %>%
  group_by(Region, ItemCode, ElementCode,  Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  # Calculating Vegetable oils and Palm oils
  filter(ItemCode %in% c("MA", "OCG", "BV", "PK", "PT", "SH", "SU", "WT", "RI", "KM", "CM", "SB", "OOS", "CSE", "PL", "KL", "CL")) %>% 
  spread(ItemCode, Value) %>% 
  # select(Region, ElementCode,  Year,  )
  mutate(
    `Coars grains` = OCG,
    Maize = MA,
    Meat =  BV + PK + PT + SH,
    Wheat = WT,
    Rice = RI,
    Sugar = SU) %>% 
  rowwise() %>% 
  mutate(
    `Protein meals` = sum(KM, CM,  OOS * (1 - 0.38), CSE * (1 - 0.15), na.rm = TRUE),
    `Soybeans` = SB,
    `Eidable oils`= sum(PL, KL, CL, OOS * 0.38, CSE * 0.15, na.rm = TRUE)) %>% 
  select(Region, ElementCode, Year, `Coars grains`, `Soybeans`, Maize, Meat, Wheat, Rice, `Protein meals`, `Eidable oils`, Sugar) %>% 
  gather(Item, Value, 4:length(.)) 

pRegData <-
  pRegData_FULL %>% 
  
  # Making average years
  left_join(yearsMT, by = "Year") %>%
  group_by(Region, Item, ElementCode,  YearAggregate) %>%
  summarise(Value = mean(Value, na.rm = TRUE) / 1000) %>%
  
  ungroup() %>%
  mutate(Type = ifelse(Item %in% c("Coars grains", "Wheat", "Rice", "Maize"), "Cereals", "Other products" )) %>%
  filter(!is.na(YearAggregate)) %>%
  spread(ElementCode, Value) %>%
  mutate(Item = as.factor(Item))

write.csv(pRegData, file = "globalTrade/output/PROD_CONS_DATA.CSV", row.names = FALSE)
# 
# 
# # Plotting one region -----------------------------------------------------
# 
# # Testing
plotData <-
  pRegData %>%
  filter(Region == "Africa", YearAggregate %in% c("2001-2005", "2011-2015", "2021-2025")) %>%
  # filter(YearAggregate %in% c("2001-2005", "2021-2025")) %>%
  mutate(NT = EX - IM) %>% 
  group_by(Region, Type) %>% 
  mutate(maxCoord = seq(from = 0, to = max(QC, QP), length.out = n()))


# 
p <-
  ggplot(plotData, aes(
    QC,
    QP)) +
  # geom_label(mapping = aes(label = Lable), vjust = "inward", hjust = "inward")+
  geom_point(mapping = aes(group = Item, colour = Item, fill = Item, shape = as.factor(YearAggregate)))  +
  geom_line(mapping = aes(group = Item, colour = Item)) + #, arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  geom_line(mapping = aes(x = maxCoord, y = maxCoord), inherit.aes = FALSE, show.legend = FALSE) +
  theme_bw() +
  # scale_color_brewer(type = "qual", palette = 7) +
  facet_wrap( ~Type, scales = "free") +
  labs(title = unique(plotData$Region),
       subtitle = str_c("Dynamic for 5 years averages from ", min(plotData$YearAggregate), " to ", max(plotData$YearAggregate), "."),
       shape = "Period",
       group = "Commodity",
       x = "Consumption, million MT",
       y = "Production, million MT") +
  theme(aspect.ratio = 1)
p
# 
# ggsave(
#   str_c(
#     "globalTrade/output/",
#     unique(plotData$Region),
#     ".png"
#   ),
#   plot = p,
#   dpi = 600
# )
# Mass plotting

d_ply(
  filter(pRegData, YearAggregate %in% c("2001-2005", "2011-2015", "2021-2025"))%>% 
    group_by(Region, Type) %>% 
    mutate(maxCoord = seq(from = 0, to = max(QC, QP), length.out = n())),
  .(Region),
  function(plotData) {
    
    p <-
      ggplot(plotData, aes(
        QC,
        QP)) +
      # geom_label(mapping = aes(label = Lable), vjust = "inward", hjust = "inward")+
      geom_point(mapping = aes(group = Item, colour = Item, fill = Item, shape = as.factor(YearAggregate)))  +
      geom_line(mapping = aes(group = Item, colour = Item)) + #, arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
      geom_line(mapping = aes(x = maxCoord, y = maxCoord), inherit.aes = FALSE, show.legend = FALSE) +
      theme_bw() +
      # scale_color_brewer(type = "qual", palette = 7) +
      facet_wrap( ~Type, scales = "free") +
      labs(title = unique(plotData$Region),
           subtitle = str_c("Dynamic for 5 years averages from ", min(plotData$YearAggregate), " to ", max(plotData$YearAggregate), "."),
           shape = "Period",
           group = "Commodity",
           x = "Consumption, million MT",
           y = "Production, million MT") +
      theme(aspect.ratio = 1)
    
    ggsave(
      str_c(
        "globalTrade/output/Production_vs_consumption_",
        unique(plotData$Region),
        ".png"
      ),
      width = 8.15, 
      height = 4.5,
      plot = p,
      dpi = 900
    )
  }, .progress = "text"
)

# Loading outlook data


# SHARE TRADED ------------------------------------------------------------

Share_in_import <-
  ol %>%
  filter(AreaCode %in% c("WLD"),
         # ItemCode %in% nutrients,
         ElementCode %in% Flows) %>%
  spread(ItemCode, Value) %>% 
  mutate(
    `Coars grains` = OCG,
    Maize = MA,
    Meat =  BV + PK + PT + SH,
    Wheat = WT,
    Rice = RI,
    Sugar = SU,
    `Soybeans` = SB,
    `Other oilseeds` =  OOS,
    `Palm oil` = PL) %>% 
  rowwise() %>% 
  select(AreaCode, ElementCode, Year, `Coars grains`, `Soybeans`, Maize, Meat, Wheat, Rice, `Other oilseeds`, `Palm oil`, Sugar) %>%
  gather(Item, Value, 4:length(.))  %>%
  spread(ElementCode, Value) %>%
  rowwise() %>% 
  mutate(Share_EX_in_QP = EX / QP * 100,
         Share_IM_in_QP = IM / QC * 100)


pp<- 
  Share_in_import %>%
  mutate(Type = ifelse(Item %in% c("Coars grains", "Wheat", "Rice", "Maize"), "Cereals", "Other products" )) %>% 
  ggplot(., aes(Year,Share_EX_in_QP, group = Item, colour = Item)) +
  geom_line()  +
  geom_vline(xintercept = 2015) +
  # scale_color_manual(values = setNames(1:length(unique(Share_in_import$Item)), unique(Share_in_import$Item))) +
  scale_color_brewer(type = "qual", palette = 3) +
  # geom_smooth()+
  labs(title = "World",
       subtitle = "% of production exported",
       colour = "Commodity",
       x = "Year",
       y = "Exported, %") +
  theme_bw() +
  theme(aspect.ratio = 0.25) + facet_wrap(~Type,dir = "v", scales = "free")

ggsave(
  str_c(
    "globalTrade/output/Export_vs_Production_World_.png"
  ),
  width = 8.15, 
  height = 4.5,
  plot = pp,
  dpi = 900
)
