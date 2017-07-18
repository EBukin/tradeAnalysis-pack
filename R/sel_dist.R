#' Select distinct for a simple usage
sel_dist <- function(x, ...) {
  x %>% 
    select_(.dots = lazyeval::lazy_dots(...)) %>% 
    distinct()
}

sel_unique <-
  function(x, ...) {
    # browser()
    x %>% 
      select_(.dots = lazyeval::lazy_dots(...)) %>% 
      as.data.frame() %>% 
      .[,names(.)] %>% 
      unique() %>% 
      sort()
  }