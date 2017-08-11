#' Join all labels to the comtrade structured dataframe 
#' 
#' @param data the dataframe with all data structured in the CT like way with `norm_ct_data()`
#' @param lang language of the lables supported options are "ru" (Russian) and "en" English
#' @param keepCodes parameter indicates that the codes of the variables should be kept
#' @param commAggMT path to the mapping table with extra commodity aggregates
#' @param rusCountries,countriesExtras path to the mapping tables.
join_labs <- function(data, 
                     lang = NA,
                     keepCodes = TRUE,
                     commAggMT = system.file("extdata", "HS_agg_names.csv", package = "tradeAnalysis"),
                     countriesExtras = system.file("extdata", "countries_extras_names.csv", package = "tradeAnalysis"),
                     rusCountries = system.file("extdata", "rus_countries.xlsx", package = "tradeAnalysis")) {
  
  data <- join_labels_com(data = data, lang = lang, keepCodes = keepCodes, commAggMT = commAggMT)
  data <- join_labels_par(data = data, lang = lang, keepCodes = keepCodes, countriesExtras = countriesExtras, rusCountries = rusCountries)
  data <- join_labels_rep(data = data, lang = lang, keepCodes = keepCodes, countriesExtras = countriesExtras, rusCountries = rusCountries)
  data <- join_labels_flows(data = data, lang = lang, keepCodes = keepCodes)
  data <- join_labels_units(data = data, lang = lang, keepCodes = keepCodes)
  data
}






