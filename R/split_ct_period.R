#' Split the Period variable in the CT data into month and year
split_ct_period <- function(.data) {
  .data %>% 
    mutate(Year = as.integer(str_sub(Period, 1,4)),
           Month = as.integer(str_sub(Period, 5,6)))
}