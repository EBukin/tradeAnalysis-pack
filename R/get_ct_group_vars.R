#' Get a list of possible grouping Variables for summarising a CT dataset
#'
#' Return a standard list of variables used for groupping
#' @param excl,include an optional vector of variables names that could be
#'   included or excluded depending on the need in analysis
get_ct_group_vars <-
  function(existingNames = NA,
           excl = NA,
           include = NA) {
    groupVars <-
      c(
        "Partner",
        "Partner.Code",
        "Reporter",
        "Reporter.Code",
        "Year",
        "Period",
        "Trade.Flow.Code",
        "Trade.Flow",
        "Commodity.Code",
        "Variable",
        "Type",
        "Commodity",
        "Unit",
        "Qty.Unit.Code",
        "Unit.Description",
        "Classification"
      )
    
    if (any(!is.na(include))) {
      groupVars <-
        stringr::str_replace_na(include, "") %>%
        c(.,groupVars)
    }
    
    if (any(!is.na(excl))) {
      groupVars <-
        groupVars[!groupVars %in% excl]
    }
    
    if(any(!is.na(existingNames))) {
      groupVars <- existingNames[existingNames %in% groupVars]
    }
    
    groupVars
    
  }