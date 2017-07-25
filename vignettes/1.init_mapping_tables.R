# Loading funcitons
library(stringr)
plyr::l_ply(str_c("R/", list.files("R/", pattern="*.R")), source)

# this is a script where we initialise the ct maping elements
reporters <- getCTReporters()

save(reporters, file = "data/reporters.rda")

partners <- 
  getCTPartners() %>% 
  bind_rows(data.frame(Partner.Code = 889L, Partner = "FSR", stringsAsFactors = FALSE),
            data.frame(Partner.Code = 888L, Partner = "ROW", stringsAsFactors = FALSE))

save(partners, file = "data/partners.rda")

regimes <- 
  getCTRegimes() %>% 
  bind_rows(data.frame(Trade.Flow.Code = 9, Trade.Flow = "Trade balance" , stringsAsFactors = FALSE))

save(regimes, file = "data/regimes.rda")

classes <- 
  bind_rows(data.frame( Commodity.Code = "01-24", Commodity = "Agriculture", stringsAsFactors = FALSE),
            getCTClass()) %>% 
  tbl_df()

save(classes, file = "data/classes.rda")

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

save(units, file = "data/units.rda")

# save(reporters, partners, regimes, classes, units, file = "data/ctClass.Rdata")
