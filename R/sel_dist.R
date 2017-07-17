#' Select distinct for a simple usage
sel_dist <- function(x, ...) {
  x %>% 
    select_(.dots = lazyeval::lazy_dots(...)) %>% 
    distinct()
}
