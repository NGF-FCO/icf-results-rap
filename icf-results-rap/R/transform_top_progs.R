#' top programmes for current year
#' 
#' @param x  summary_main
#' 

  transform_top_progs <- function(x){
    
    y <- x %>% 
          select(kpi_id, programme_id, programme_title, 
                 intersect(
                   contains(as.character(year(Sys.Date()))), 
                           starts_with("achieved")
                          )
                 ) %>% 
          group_by(programme_id, programme_title, kpi_id) %>% 
          summarise(across(starts_with("achieved"), \(x)sum(x, na.rm=T), .names = "tot")) %>% 
          group_by(kpi_id) %>% 
          slice_max(order_by = tot, n = 5) %>% 
          filter(tot!=0) %>% 
          ungroup() %>% 
          mutate_if(is.factor, as.character)
    
    return(y)
  
  }