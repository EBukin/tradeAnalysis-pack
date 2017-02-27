# Analyzing trade potential by regions


# Setups ------------------------------------------------------------------

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidyverse)


# Loading data ------------------------------------------------------------

file <- "globalTrade/baseline2016.csv"

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
  c(
    "AFL",
    "AFN",
    "AFS",
    "DZA",
    "EGY",
    "ETH",
    "GHA",
    "MOZ",
    "NGA",
    "SDN",
    "TZA",
    "ZAF",
    "ZMB",
    # Sub Sahara Africa
    "AFL",
    "AFS",
    "ETH",
    "GHA",
    "MOZ",
    "NGA",
    "SDN",
    "TZA",
    "ZAF",
    "ZMB"
  )

AsiaOceania <-
  c(
    "CHN",
    "JPN",
    "KOR",
    "ASA",
    "ASD",
    "ASL",
    "BGD",
    "IDN",
    "IND",
    "IRN",
    "MLE",
    "MYS",
    "PAK",
    "PHL",
    "SAU",
    "THA",
    "TUR",
    "VNM",
    "ISR",
    "KAZ",
    # Oceania
    "AUS",
    "NZL",
    "OCE",
    "OCL"
  )

Europe <-
  c("CHE", "EUN", "NOR", "RUS", "EUE", "EUW", "UKR",
    # EU
    "E15", "NMS")

NorthAmerica <-
  c("USA", "CAN")

SouthAmerica <-
  c("ARG",
    "BRA",
    "MEX",
    "CHL",
    "COL",
    "PER",
    "PRY",
    "SAC",
    "URY",
    "HTI")

# Initializing regions mapping table
regionsMT <-
  bind_rows(
    tibble(Region = "Africa", AreaCode = Africa),
    tibble(Region = "Asia and Oceania", AreaCode = AsiaOceania),
    tibble(Region = "Europe", AreaCode = Europe),
    tibble(Region = "North America", AreaCode = NorthAmerica),
    tibble(Region = "South America", AreaCode = SouthAmerica),
    tibble(Region = "World", AreaCode = World)
  )

# Year mapping table
yearsMT <-
  tibble(YearAggregate = sort(rep(str_c(
    seq(2001, 2021, 5), seq(2005, 2025, 5), sep = "-"
  ), 5)), Year = 2001:2025)

# Commodities
Cereals <- c("OCG", "RI", "WT", "MA")
OilSugar <- c("SB", "OOS", "SU")
Meat <- c("BV", "PT", "PK", "SH")

itemsMT <-
  bind_rows(
    tibble(
      type = "Crops",
      Item = c("Other coarse grains", "Rice", "Wheat", "Maize"),
      ItemCode = Cereals
    ),
    tibble(
      type = "Crops",
      Item = c("Soybeans", "Other oil seeds", "Sugar"),
      ItemCode = OilSugar
    ),
    tibble(
      type = "Meat",
      Item = c("Beaf and veal", "Poultry", "Pork", "Sheep"),
      ItemCode = Meat
    )
  )

# Flows
Flows <- c("QP", "QC", "IM", "EX", "ST")

# Nutrients
nutrient <- c("PC", "PCCalo", "PCProt", "PCFat")

# Analysis ----------------------------------------------------------------

# Filtering data
pRegData <-
  ol %>%
  filter(AreaCode %in% World,
         ItemCode %in% c(Cereals, OilSugar, Meat),
         ElementCode %in% Flows) %>%
  
  # Agg regions
  left_join(regionsMT, by = "AreaCode") %>%
  group_by(Region, ItemCode, ElementCode,  Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  
  # Making average years
  left_join(yearsMT, by = "Year") %>%
  group_by(Region, ItemCode, ElementCode,  YearAggregate) %>%
  summarise(Value = mean(Value, na.rm = TRUE) / 1000) %>%
  
  ungroup() %>%
  filter(!is.na(YearAggregate)) %>%
  spread(ElementCode, Value) %>%
  left_join(itemsMT, "ItemCode") %>%
  mutate(Lable = ifelse(YearAggregate == "2021-2025", Item, NA)) %>%
  mutate(Item = as.factor(Item))


# Plotting one region -----------------------------------------------------


# Testing
plotData <-
  pRegData %>%
  filter(Region == "Europe", type == "Crops") %>%
  filter(YearAggregate %in% c("2001-2005", "2021-2025"))

p <-
  ggplot(plotData, aes(
    QC,
    QP,
    group = Item,
    colour = Item,
    fill = Item
  )) +
  # geom_label(mapping = aes(label = Lable), vjust = "inward", hjust = "inward")+
  geom_point() +
  geom_line(arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  geom_abline(mapping = aes(intercept = 0, slope = 1))  +
  theme_bw() +
  # scale_y_log10()+
  # scale_x_log10()+
  expand_limits(x = c(1, max(plotData$QC, plotData$QP)),
                y = c(1, max(plotData$QC, plotData$QP))) +
  labs(title = unique(plotData$Region),
       x = "Consumption, million tones",
       y = "Production, million tones") +
  scale_color_brewer(type = "qual", palette = 2)

ggsave(
  str_c(
    "globalTrade/",
    unique(plotData$Region),
    "_",
    unique(plotData$type),
    ".png"
  ),
  plot = p,
  width = 12,
  height = 8,
  units = "cm",
  dpi = 600
)
# Mass plotting

d_ply(
  #filter(pRegData, YearAggregate %in% c("2001-2005", "2021-2025")), 
  pRegData,
      .(type, Region),
      function(plotData) {
        
        p <-
          ggplot(plotData, aes(
            QC,
            QP,
            group = Item,
            colour = Item,
            fill = Item
          )) +
          # geom_label(mapping = aes(label = Lable), vjust = "inward", hjust = "inward")+
          geom_point() +
          geom_line(arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
          geom_abline(mapping = aes(intercept = 0, slope = 1))  +
          theme_bw() +
          # scale_y_log10()+
          # scale_x_log10()+
          expand_limits(x = c(1, max(plotData$QC, plotData$QP)),
                        y = c(1, max(plotData$QC, plotData$QP))) +
          labs(title = unique(plotData$Region),
               x = "Consumption, million tones",
               y = "Production, million tones") +
          scale_color_brewer(type = "qual", palette = 2)
        
        ggsave(
          str_c(
            "globalTrade/",
            unique(plotData$Region),
            "_",
            unique(plotData$type),
            ".png"
          ),
          plot = p,
          dpi = 600
        )
      }, .progress = "text"
      )

# Loading outlook data
