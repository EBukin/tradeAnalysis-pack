#' Normalizing data
norm_ct_data <- function(df) {
  if (nrow(df) > 0) {
    df %>%
      dplyr::select_(.dots = names(.)[!names(.) %in% c("Flag", "Qty.Unit.Code", "Qty")]) %>%
      tidyr::gather(Variable, Value, Netweight..kg., Trade.Value..US.., na.rm = TRUE) %>%
      dplyr::mutate(Variable = as.factor(Variable)) %>%
      return()
  }
}