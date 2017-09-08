#' Loading some data from the CT data files slorred in Rdata format locally
load_some_ct_bulks_rdata <- 
  function(fromFolder = "data_raw/ctMnAll/",
           filesList,
           hsCodes = NA,
           reporters = NA,
           partners = NA,
           tradeFlows = c(1,2)) {
    
    map_df(filesList,
           function(x) {
             
             oneFilePath <- file.path(fromFolder, x)
             
             if (file.exists(oneFilePath)) {
               loadedObject <- load(file = oneFilePath, verbose = F)
               
               if (any(!is.na(tradeFlows))) {
                 assign(loadedObject,
                        get(loadedObject) %>%
                          filter(Trade.Flow.Code %in% tradeFlows))
               }
               
               if (any(!is.na(hsCodes))) {
                 assign(loadedObject,
                        get(loadedObject) %>%
                          filter(Commodity.Code %in% hsCodes))
               }
               
               if (any(!is.na(reporters))) {
                 assign(loadedObject,
                        get(loadedObject) %>%
                          filter(Reporter.Code %in% reporters))
               }
               
               if (any(!is.na(partners))) {
                 assign(loadedObject,
                        get(loadedObject) %>%
                          filter(Partner.Code %in% partners))
               }
               
               gc()
             } else {
               warning(paste0("File '", oneFilePath, "' does not exist and cannot be loaded."))
               loadedObject <- NULL
             }
             
             get(loadedObject)
           })
    
  }
