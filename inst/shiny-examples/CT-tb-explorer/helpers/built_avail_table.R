built_avail_table <- function(dat) {
  if (!is.null(dat)) {
    if (nrow(dat) > 0) {
      if ("Month" %in% names(dat)) {
        if (all(is.na(dat$Month))) {
          tabll <-
            dat %>% 
            select(-Month) %>%
            spread(Year, Type) %>% 
            rename(Code = Reporter.Code) 
        } else {
          tabll <-
            dat %>% 
            mutate(Month = as.character(Month),
                   Month = ifelse(str_length(Month) == 1, str_c("0", Month), Month)) %>% 
            unite(Period, Year, Month) %>%
            spread(Period, Type) %>% 
            rename(Code = Reporter.Code) 
        }
      } else {
        tabll <- dat %>%
          spread(Year, Type) %>% 
          rename(Code = Reporter.Code) 
      }
      # browser()
      suppressWarnings(
        tabll <-
          tabll %>%
          datatable(
            # rownames = FALSE,
            # extensions = 'FixedColumns',
            options = list(
              autoWidth = TRUE,
              # fixedColumns = list(leftColumns = 3),
              scrollX = TRUE
            )))
      tabll <-
        tabll %>%
        formatStyle(1:length(tabll$x$data),
                    backgroundColor = styleEqual(c("Direct", "Mirror"), c("#34A853", "#FBBC05")))
    } else {
      tabll <- NULL
    }
  } else {
    tabll <- NULL
  }
  tabll
}