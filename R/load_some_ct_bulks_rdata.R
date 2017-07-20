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
             
             loadedObject <- load(file = file.path(fromFolder, x), verbose = F)
             
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
             get(loadedObject)
           })
    
  }
