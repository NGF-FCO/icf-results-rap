#' make ta kpi 1 for publication table
#' 
#' @param x ta kpi summary target
#' 


table_ta01 <- function(x){
  
  y <- x %>% 
          group_by(period_name) %>% 
          mutate(id = row_number()) %>% 
          pivot_wider(names_from = "period_name", values_from = "disagg_country", id_cols="id") %>% 
          ungroup() %>% 
          select(-id, -matches("2012")) %>% 
          remove_empty(which = "rows") %>% 
          bind_rows(map_chr(., function(x)sum(!is.na(x)) %>% as.character())) 
       
  return(y)
  
}
      


  
   