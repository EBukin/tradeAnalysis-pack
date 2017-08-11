#' @describeIn join_labs join lables to the reporters codes
join_labels_rep <- function(data,
                            lang = NA,
                            keepCodes = TRUE,
                            countriesExtras = system.file("extdata", "countries_extras_names.csv", package = "tradeAnalysis"),
                            rusCountries = system.file("extdata", "rus_countries.xlsx", package = "tradeAnalysis")) {
  if (!is.na(lang) &
      stringr::str_detect(lang, stringr::regex("ru", ignore_case = TRUE))) {
    rusCountriesMT <-
      readxl::read_excel(rusCountries, sheet = 1, col_names = TRUE) %>%
      dplyr::select(Code, Name)
    mappingTable <-
      rusCountriesMT %>%
      distinct()
  } else {
    extraCountries <- readr::read_csv(countriesExtras,
                                      col_types = cols(Code = col_integer(),
                                                       Name = col_character()))
    mappingTable <-
      bind_rows(
        dplyr::rename(
          tradeAnalysis::partners,
          Code = Partner.Code,
          Name = Partner
        ),
        dplyr::rename(
          tradeAnalysis::reporters,
          Code = Reporter.Code,
          Name = Reporter
        ),
        extraCountries
      ) %>%
      distinct()
  }
  join_labels_generic(
    data = data,
    mappingTable = mappingTable,
    oldNames = c("r", "Reporter.Code"),
    newNames = c("Reporter"),
    keepCodes = keepCodes
  )
}