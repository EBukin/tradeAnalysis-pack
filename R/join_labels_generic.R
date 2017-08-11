#' @describeIn join_labs join lables to the codes of a comtrade based dataframe (generic)join_labs
#' @param oldNames,newName character vectors with the names of the old variables 
#'     to be remover of used as the coding one and the new variables to be incerted
join_labels_generic <- function(data, mappingTable, oldNames, newNames, keepCodes, ...) {
  # Join mapping table if the field exis
  if (any(oldNames %in% names(data))) {
    codeVar <- oldNames[oldNames %in% names(data)][1]
    data <-
      data %>%
      select_(.dots = names(.)[!names(.) %in% newNames])
    data <-
      match_labels_to_var_class(
        data = data,
        mappingTable = mappingTable,
        codeVar = codeVar,
        resultingNameVar = newNames
      )
    if (!keepCodes) {
      data <-
        data %>%
        select_(.dots = one_of(oldNames))
    }
  }
  data
}