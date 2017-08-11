#' Join mapping table lables to the origical file miantaining the original calsses structure
#'
#' @param data the dataframe with all data structured in the CT like way with `norm_ct_data()`
#' @param mappingTable the data frame with the maping table, which contains columns Code and Name
#' @param codeVar string name of the variable with the code that is used for joining the mapping table.
#' @param resultingNameVar string name of the resulting variable (Partner, Commodity)
match_labels_to_var_class <-
  function(data,
           mappingTable,
           codeVar,
           resultingNameVar) {
    mutateCall <-
      lazyeval::interp( ~ ifelse(is.na(x), y, x),
                        x = as.name(resultingNameVar),
                        y = as.name(codeVar))
    if (class(data[, codeVar][[1]]) != class(mappingTable[, "Code"][[1]])) {
      classType <- class(data[, codeVar][[1]])
      mappingTable <- 
        mappingTable %>%
        dplyr::mutate(Code = as(Code, classType)) 
    } 
    data <-
      data %>%
      left_join(
        mappingTable %>%
          dplyr::rename_(.dots = setNames("Name", resultingNameVar)),
        by = setNames("Code", codeVar)
      ) %>%
      mutate_(.dots = setNames(list(mutateCall), resultingNameVar))
    data
  }