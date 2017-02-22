#' Normalizing data
norm_ct_data <- function(df) {
  df %>% 
    dplyr::select(-Flag, - Qty.Unit.Code, - Qty) %>% 
    tidyr::gather(Variable, Value, Netweight..kg., Trade.Value..US.., na.rm = TRUE) %>% 
    dplyr::mutate(Variable = as.factor(Variable)) %>% 
    return()
}