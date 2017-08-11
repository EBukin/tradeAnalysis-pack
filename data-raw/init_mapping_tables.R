# Script for initializing all data tables used in further analysis

library(tidyverse)
library(stringr)
plyr::l_ply(str_c("R/", list.files("R/", pattern="*.R")), source)

# Reporters ---------------------------------------------------------------

reporters <- getCTReporters()
devtools::use_data(reporters, overwrite = TRUE)

# Partners ----------------------------------------------------------------

partners <- 
  getCTPartners() %>% 
  bind_rows(data.frame(Partner.Code = 889L, Partner = "FSR", stringsAsFactors = FALSE),
            data.frame(Partner.Code = 888L, Partner = "ROW", stringsAsFactors = FALSE))
devtools::use_data(partners, overwrite = TRUE)


# Trade flows -------------------------------------------------------------

regimes <- 
  getCTRegimes() %>% 
  bind_rows(data.frame(Trade.Flow.Code = 9, Trade.Flow = "Trade balance" , stringsAsFactors = FALSE))
devtools::use_data(regimes, overwrite = TRUE)


# HS classification -------------------------------------------------------

classes <- 
  getCTClass() %>% 
  tbl_df()
devtools::use_data(classes, overwrite = TRUE)

# Units -------------------------------------------------------------------

units <-
  data.frame(
    Qty.Unit.Code = c(1:13),
    Unit = c(
      "-", "m2", "1000 kWh", "m", "u", "2u", "l", "kg", "1000U", "U (jeu/pack)", "12u", "m3", "carat"
    ),
    Unit.Description =
      c(
        "No quantity", "Area in square meters", "Electrical energy in thousands of kilowatt-hours",
        "Length in meters", "Number of items", "Number of pairs", "Volume in liters",
        "Weight in kilograms", "Thousand of items", "Number of packages",
        "Dozen of items", "Volume in cubic meters", "Weight in carats"
      ),
    stringsAsFactors = FALSE
  )
devtools::use_data(units, overwrite = TRUE)
