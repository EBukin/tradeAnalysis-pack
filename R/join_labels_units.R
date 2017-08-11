#' @describeIn join_labs join lables of the unit code
join_labels_units <-
  function(data,
           lang = NA,
           keepCodes = TRUE) {
    mappingTable <- tradeAnalysis::units[, c(1, 2)]
    names(mappingTable)[1] <- "Code"
    names(mappingTable)[2] <- "Name"
    # Join mapping table if the field exis
    join_labels_generic(
      data = data,
      mappingTable = mappingTable,
      oldNames = c("Qty.Unit.Code"),
      newNames = c("Unit"),
      keepCodes = keepCodes
    )
  }
