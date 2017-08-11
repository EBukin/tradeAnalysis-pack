#' @describeIn join_labs join lables of the trade flow code
join_labels_flows <-
  function(data,
           lang = NA,
           keepCodes = TRUE) {
    mappingTable <- tradeAnalysis::regimes
    names(mappingTable)[1] <- "Code"
    names(mappingTable)[2] <- "Name"
    # Join mapping table if the field exis
    join_labels_generic(
      data = data,
      mappingTable = mappingTable,
      oldNames = c("Trade.Flow.Code"),
      newNames = c("Trade.Flow"),
      keepCodes = keepCodes
    )
  }