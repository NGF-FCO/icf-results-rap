#' produces publication table for writing to csv 
#' 
#' @param x  summary_cumsum in pipeline
#' @param periods  summary target in pipeline
#' 


table_cumsum <- function(x, periods){
  
  y <- x %>% 
          pivot_wider(id_cols = c(kpi_id, kpi_title), names_from = year, values_from = achieved_cumulative) %>% 
          mutate_if(is.numeric, ~case_when(
            .x==0~NA,
            .x>10000 ~ round_choose(.x,1000),
            TRUE ~ .x
          )) %>% 
          rename_at(vars(-c(kpi_id,kpi_title)), ~periods) %>% 
          rename("KPI Number"=kpi_id) %>%
          rename("KPI Name"=kpi_title) 
      
  return(y)
  
}

