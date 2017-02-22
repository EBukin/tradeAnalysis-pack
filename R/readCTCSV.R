#' Read one CT bulk csv file previously unzipped from the archive
readCTCSV <-
  function(path) {
    
    require(dplyr)
    
    # Reading data
    df <- data.table::fread(
      path,
      sep = ",",
      header = TRUE,
      stringsAsFactors = FALSE,
      integer64 = "numeric",
      colClasses = "character"
    ) %>%
      tbl_df()
    
    # correcting the encoding error in the header names of the table
    names(df) <-
      make.names(iconv(names(df), "latin1", "ASCII", sub = ""))
    
    df <- df
    
    if (ncol(df) > 10) {
      df %>%
        select(
          Classification, Year, Period, Trade.Flow.Code,
          Reporter.Code, Partner.Code,
          Commodity.Code, Qty.Unit.Code,
          Qty, Netweight..kg., Trade.Value..US.., Flag
        ) %>%
        mutate_each(
          funs(as.integer),
          Year, Trade.Flow.Code,
          Reporter.Code, Partner.Code,
          Qty.Unit.Code, Flag
        ) %>%
        mutate_each(funs(as.numeric),
                    Qty, Netweight..kg., Trade.Value..US..) %>%
        return()
    }
    
  }