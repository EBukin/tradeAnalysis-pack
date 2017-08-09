#' Get CT bulk data availability from the web
#'
getCTParameters <- function() {
  jsonlite::fromJSON("http://comtrade.un.org/api/refs/da/bulk?parameters") %>%
    tbl_df() %>%
    mutate(
      year = str_sub(ps, start = 1, end = 4),
      month = stringr::str_sub(ps, start = 5, end = 6),
      month = ifelse(month == "" & freq == "ANNUAL", "AN", month),
      publicationDate = as.Date(str_sub(
        publicationDate, start = 1, end = 10
      )),
      extractDate = as.Date(str_sub(
        extractDate, start = 1, end = 10
      ))
    )
}