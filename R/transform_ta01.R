#' transform Technical Assistance KPI 1
#' extracts annual results for TA KPI 1 in long format.
#' 
#' @param x  rex target
#' @param period  reporting_periods target


transform_ta01 <- function(x, period){ 
  
  y <- x %>% 
          filter(kpi_id=="ICF TA KPI 01" & period_name %in% period) %>% 
          group_by(period_name) %>% 
          distinct(disagg_country, .keep_all = T) %>% 
          arrange(disagg_country, .by_group = T) %>% 
          select(disagg_country, period_name) %>% 
          filter(!is.na(disagg_country)) %>% 
          ungroup() 
  
  return(y)
    
}


