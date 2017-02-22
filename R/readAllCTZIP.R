#' Reading all CT zip achive in one folder
readAllCTZIP <-
  function(folder, parallel = FALSE, delete = TRUE, ...) {
    require(plyr)
    require(stringr)
    
    files <- listCTdata(folder)
    
    data <-
      ddply(
        .data = files,
        .variables = .(name),
        .fun = function(x) {
          readCTZIP(file = x$name, folder)
        },
        .parallel = parallel,
        .progress = "text"
      ) %>%
      tbl_df()
    
    data
    
  }
