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
             
             assign(loadedObject,
                    eval(parse(text = loadedObject)) %>%
                      filter(Trade.Flow.Code %in% tradeFlows))
             
             if (all(!is.na(hsCodes))) {
               assign(loadedObject,
                      eval(parse(text = loadedObject)) %>%
                        filter(Commodity.Code %in% hsCodes))
             }
             
             gc()
             if (all(!is.na(reporters))) {
               assign(loadedObject,
                      eval(parse(text = loadedObject)) %>%
                        filter(Reporter.Code %in% reporters))
             }
             
             gc()
             if (all(!is.na(partners))) {
               assign(loadedObject,
                      eval(parse(text = loadedObject)) %>%
                        filter(Partner.Code %in% partners))
             }
             
             gc()
             eval(parse(text = loadedObject))
           })
    
  }
