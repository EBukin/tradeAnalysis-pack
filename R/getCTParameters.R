#' Get CT bulk data availability from the web
#'
getCTParameters <- function(token = NA) {
  request <- "http://comtrade.un.org/api/refs/da/bulk?parameters"
  if (!is.na(token)) 
    request <- stringr::str_c("https://comtrade.un.org/api/refs/da/bulk?token=", token)
  jsonlite::fromJSON(request) %>%
    tbl_df() %>%
    mutate(
      year = stringr::str_sub(ps, start = 1, end = 4),
      month = stringr::str_sub(ps, start = 5, end = 6),
      month = ifelse(month == "" & freq == "ANNUAL", "AN", month),
      publicationDate = as.Date(stringr::str_sub(
        publicationDate, start = 1, end = 10
      )),
      extractDate = as.Date(stringr::str_sub(
        extractDate, start = 1, end = 10
      ))
    )
}