# Author: EduardBukin
# Purpose: Identifting the list of HS codes used for agricultural commodities


# Setups -----------------------------------------------------------------


require("tidyverse")
require("stringr")
require("knitr")
require("xlsx")
require("grid")

l_ply(str_c("R/", list.files("R/", pattern="*.R")), source)


# Initilizing the HS codes ------------------------------------------------

# We initialize HS codes according to the WTO Agreememnt's annex 1 https://www.wto.org/english/docs_e/legal_e/14-ag_02_e.htm#annI
# 2905.43
# 2905.44
# 33.01
# 35.01 to 35.05
# 3809.10
# 3823.60
# 41.01 to 41.03
# 43.01
# 50.01 to 50.03
# 51.01 to 51.03
# 52.01 to 52.03
# 53.01
# 53.02

ag_general <- tibble(cc1 = c(str_c("0", c(1:9)), as.character(c(10:24))), prime = TRUE)
# fish <- tibble(cc1 = c("03"))
special <- 
  tibble(cc1 = c("29", "29", "33", "35", "35", "35", "35", "35", "38", "38", "41", 
                 "41", "41", "43", "50", "50", "50", "51", "51", "51", "52", "52", "52", "53", "53"), 
         cc2 = c("05", "05", "01", "01", "02", "03", "04", "05", "09", "23", "01", 
                 "02", "03", "01", "01", "02", "03", "01", "02", "03", "01", "02", "03", "01", "02"), 
         cc3 = c("43", "44",  "" ,  "" ,   "",   "",   "",   "",   "",   "", "10", 
                 "60",   "",   "",   "",   "",   "",   "",   "",   "",   "",   "",   "",   "",   ""),
         prime = TRUE)


# Loading all HS codes and descriptions
all_ct <-
  getCTClass() %>%
  mutate(
    cc1 = str_sub(Commodity.Code, 1, 2),
    cc2 = str_sub(Commodity.Code, 3, 4),
    cc3 = str_sub(Commodity.Code, 5, 6)
  )

wtoAgFoodFull <-
  bind_rows(
    left_join(ag_general, all_ct, by = "cc1") %>% filter(cc2 == ""),
    left_join(ag_general, all_ct, by = "cc1") %>% filter(cc2 != "", cc3 == "") %>% mutate(prime = FALSE),
    left_join(ag_general, all_ct, by = "cc1") %>% filter(cc2 != "", cc3 != "") %>% mutate(prime = FALSE),
    left_join(filter(special, cc3 == "") %>% select(-cc3), all_ct, by = c("cc1", "cc2")) %>% filter(cc3 == ""),
    left_join(filter(special, cc3 == "") %>% select(-cc3), all_ct, by = c("cc1", "cc2")) %>% filter(cc3 != "") %>% mutate(prime = FALSE),
    left_join(filter(special, cc3 != ""), all_ct, by = c("cc1", "cc2", "cc3"))
  ) %>%
  mutate(Commodity.Code = ifelse(is.na(Commodity.Code), str_c(cc1, cc2, cc3), Commodity.Code)) %>% 
  select(cc1, cc2, cc3, prime, Commodity.Code, Commodity, prime)

wtoAgFood <-
  wtoAgFoodFull %>%
  filter(prime) %>%
  select(-prime)


# Saving data -------------------------------------------------------------

save(wtoAgFoodFull, wtoAgFood, file = "data/wtoAgFood.Rdata")
save(wtoAgFoodFull, file = "data/wtoAgFoodFull.rda")
save(wtoAgFood, file = "data/wtoAgFood.rda")

# Writing an excel file
write.xlsx2(
  as.data.frame(wtoAgFood),
  file = "output/WTO_AgFood_hs_list_EB.xlsx",
  sheetName = "Key AgfFood list",
  row.names = FALSE,
  append = FALSE
)

write.xlsx2(
  as.data.frame(wtoAgFoodFull) %>%
    select(-prime),
  file = "output/WTO_AgFood_hs_list_EB.xlsx",
  sheetName = "Full AgfFood list",
  row.names = FALSE,
  append = TRUE
)
