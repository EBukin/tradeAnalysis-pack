library(tidyverse)
library(readr)
library(tradeAnalysis)

fix_codes <- function(dat) {
  dat %>% 
    # Group 1
    mutate(Commodity.Agg = if_else(Commodity.Agg == "WTO_AgriFood", "00__WTO_AgriFood_HS2", Commodity.Agg))  %>% 
    # Group 2
    mutate(Commodity.Agg = if_else(Commodity.Agg == "1_24_Total_excl_fish", "01__1_24_Total_excl_fish_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "3_Fish", "01__3_Fish_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "29_38_Chemicals", "01__29_38_Chemicals_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "41_43_Skins", "01__41_43_Skins_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "50_Silk", "01__50_Silk_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "51_Wool", "01__51_Wool_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "52_Cotton", "01__52_Cotton_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "53_Fibers", "01__53_Fibers_HS2", Commodity.Agg)) %>% 
    
    # Group 3
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "1_05_Animal", "02__1_5_Animal_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "3_Fish_Anim", "02__3_Fish_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "06_15_Vegetable", "02__6_15_Vegetable_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "16_24_Food_products", "02__16_24_Food_products_HS2", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "29_53_AgriGoods", "02__29_53_Other_agri_goods_HS2", Commodity.Agg)) %>% 
    
    # Group 4
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "01-03-Animal, Meat and Fish", "03__1-3_5_Meat_fish_egg_honey_animal_HS4", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "04-Dairy", "03__4_Dairy_HS4", Commodity.Agg)) %>%
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "14,24-53-Tobacco, skins, fibers and other agicultural goods", "03__5_14_24-53_Tobacco_skins_fibers_other_ag_HS4", Commodity.Agg)) %>%
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "5,14,24-53-Tobacco, skins, fibers and other agicultural goods", "03__5_14_24-53_Tobacco_skins_fibers_other_ag_HS4", Commodity.Agg)) %>%
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "06-07-Fruts, vegetables and other products", "03__6-9_13_Fruts_vegetables_others_HS4", Commodity.Agg)) %>%
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "10-Cereals", "03__10_11_Cereals_flours_HS4", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "12-Oilseeds","03__12_13_Oilseeds_HS4", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "11,16,18-23-Food and baverage", "03__16_18-23_Food_baverage_HS4", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "15-Animal and vegetable fats", "03__15_Animal_veg_fats_HS4", Commodity.Agg)) %>% 
    mutate(Commodity.Agg = ifelse(Commodity.Agg == "17-Sugar", "03__17_Sugar_HS4", Commodity.Agg)) 
}

# Fixing coding of aggregates in the mapping tabe
readr::read_csv("inst/extdata/old/HS_agg_mt.csv") %>%
  select(-Commodity) %>%
  fix_codes %>% 
  separate(Commodity.Agg, into = c("grout", "code"),sep = "__", remove = FALSE) %>% 
  arrange(grout, Commodity.Code) %>% 
  select(-grout, -code) %>% 
  readr::write_csv("inst/extdata/HS_agg_mt.csv")

# Fixing coding of aggregates in names tabe
readr::read_csv("inst/extdata/old/HS_agg_names.csv") %>%
  rename(Commodity.Agg = Commodity.Code) %>%
  # select(-Commodity)  %>%
  fix_codes() %>% 
  add_row(Commodity.Agg = "02__3_Fish_HS2") %>% 
  arrange(Commodity.Agg) %>%
  rename(Commodity.Code = Commodity.Agg) %>% 
  readr::write_csv("inst/extdata/HS_agg_names.csv")

# Fixing coding of aggregates in groups
readr::read_csv("inst/extdata/HS_agg_mt.csv") %>% 
  sel_dist(Commodity.Agg) %>% 
  separate(Commodity.Agg, into = c("Group", "code"),sep = "__", remove = FALSE) %>% 
  select(-code) %>% 
  mutate(Group = stringr::str_c("WTO Agrifood level ", Group))  %>% 
  readr::write_csv("inst/extdata/HS_agg_groups.csv")

