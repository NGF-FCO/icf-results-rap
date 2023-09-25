#' number of KPIs reported for each programme and last year they reported. 
#' 
#' @param x  summary_main target in pipeline


transform_prog_kpi_count <- function(x, include_last_year = T){

  if(include_last_year == T) {
  
  y <- x %>% 
    select(-contains("disagg")) %>% 
    filter(achieved_total > 0 | is.na(achieved_total)) %>% 
    mutate_all( ~replace_na(.,-Inf)) %>% 
    mutate(latest_year_reported = 
            names(
              select(., intersect(matches("/"), starts_with("achieved_")))
              )[max.col(select(., intersect(matches("/"), starts_with("achieved_"))) != 0, 'last')] 
           ) %>% 
    group_by(programme_title, programme_id, latest_year_reported) %>%
    summarise(kpi_ids = str_c(unique(kpi_id), collapse=", ")) %>% 
    mutate(count = str_count(kpi_ids, ",")+1) 
  
  } else {
    
    y <- x %>% 
      select(-contains("disagg")) %>% 
      filter(achieved_total > 0 | is.na(achieved_total)) %>% 
      group_by(programme_title, programme_id) %>%
      summarise(kpi_ids = str_c(unique(kpi_id), collapse=", ")) %>% 
      mutate(count = str_count(kpi_ids, ",")+1)
  }
    
   return(y)
  
}

