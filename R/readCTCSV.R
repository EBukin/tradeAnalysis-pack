#' Read one CT bulk csv file previously unzipped from the archive
#' 
#' @param path CT bulk donwload file that is needed.
#'  
readCTCSV <-
  function(path) {
    
    require(tidyverse)
    
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
        mutate_at(
          .vars = vars(
            Year,
            Trade.Flow.Code,
            Reporter.Code,
            Partner.Code,
            Qty.Unit.Code,
            Flag
          ),
          .funs = funs(as.integer)
        ) %>%
        mutate_at(
          .vars = vars(Qty, Netweight..kg., Trade.Value..US..) ,
          .funs = funs(as.numeric)
        ) %>%
        return()
    }
    
  }