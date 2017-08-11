#' @describeIn join_labs join lables of the commodity codes
join_labels_com <-
  function(data,
           lang = NA,
           keepCodes = TRUE,
           commAggMT = system.file("extdata", "HS_agg_names.csv", package = "tradeAnalysis")) {
    mappingTable <-
      bind_rows(tradeAnalysis::classes,
                read.csv(commAggMT, stringsAsFactors = FALSE))
    names(mappingTable)[1] <- "Code"
    names(mappingTable)[2] <- "Name"
    # Join mapping table if the field exis
    join_labels_generic(
      data = data,
      mappingTable = mappingTable,
      oldNames = c("Commodity.Code"),
      newNames = c("Commodity"),
      keepCodes = keepCodes
    )
  }