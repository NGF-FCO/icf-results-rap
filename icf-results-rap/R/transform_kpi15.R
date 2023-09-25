#' get KPI 15 table from summary table
#' 
#' @param x  summary_main target
#' @param years reporting_years
#' @param periods reporting_periods
#' @param scores kpi15_scores 

transform_kpi15 <- function(x, periods, years, scores){ 

  
  call = scores %>% 
            mutate(conditions = paste0("programme_id", "==", programme_id, " & ", "period", "==", "'", period, "'", " ~ ", score)) %>% 
            pull(conditions) %>% 
            str_c(collapse = ", ") %>% 
            str_c("case_when(",.,", TRUE ~ achieved)")
          
  
  y <- x %>% 
          filter(kpi_id=="ICF KPI 15") %>% 
          select(programme_title, programme_id,
                 intersect(
                           contains(periods),
                           starts_with("achieved")
                          )
                 ) %>%
          pivot_longer(-c(programme_id, programme_title), 
                       names_to = "period", 
                       values_to = "achieved", 
                       names_prefix = "achieved_") %>% 
    mutate(achieved = 
             case_when(
               achieved == 0 ~ NA,
               TRUE ~ achieved)
           ) %>% 
    mutate(achieved = !!rlang::parse_quo(call, env = rlang::caller_env())) %>%  
    mutate(scale = case_when(
                str_detect(period, glue_collapse(years[as.numeric(years) < 2022], "|")) ~ "old",
                TRUE ~ "new"
                )
           ) %>% 
    filter(!is.na(achieved)) %>% 
    mutate(achieved = as.factor(achieved)) 
  
  return(y)
    
}