#' calculate disaggregation summary for KPIs
#' 
#' @param x  summary main
#' @param kpi which kpi to be disaggregated - 15 and TA01 will only count progs that have disaggregated

disagg <- function(x){

  kpi <- unique(x$kpi_id)
  
  disaggs <- str_split_1(unique(x$disagg_publication), pattern = ",") %>% str_trim()
  
  y <- if(kpi=="ICF KPI 01"){
              x %>% 
                filter(kpi_id==kpi & (disagg_direct_indirect_beneficiary=="Direct" | is.na(disagg_direct_indirect_beneficiary))) %>%
                filter(achieved_total>0) %>% 
                select(programme_id, all_of(disaggs), achieved_total) %>% 
                select(programme_id, where(~n_distinct(.) > 1)) %>% 
                pivot_longer(-c(programme_id, achieved_total),names_to = "disagg", values_to = "category") %>% 
                group_by(disagg, category) %>% 
                summarise(
                  achieved_total = sum(achieved_total, na.rm=T),
                  progs = n_distinct(programme_id)
                ) %>% 
              filter(!is.na(category)) %>% 
              mutate(category = tolower(category))
    
          }else{
            
                  if(kpi=="ICF KPI 15" | kpi=="ICF TA KPI 01"){
                      x %>% 
                        filter(kpi_id==kpi) %>%
                        filter(achieved_total>0) %>%  
                        select(programme_id, all_of(disaggs)) %>% 
                        select(programme_id, where(~n_distinct(.) > 1)) %>%  
                        pivot_longer(-c(programme_id),names_to = "disagg", values_to = "category") %>% 
                        group_by(disagg, category) %>% 
                        summarise(
                          progs = n_distinct(programme_id)
                          ) %>% 
                        filter(!is.na(category)) %>% 
                        mutate(category = tolower(category))
                    
                }else{
                  
                  x %>% 
                    filter(kpi_id==kpi) %>%
                    filter(achieved_total>0) %>% 
                    select(programme_id, all_of(disaggs), achieved_total) %>% 
                    select(programme_id, where(~n_distinct(.) > 1)) %>% 
                    pivot_longer(-c(programme_id,achieved_total),names_to = "disagg", values_to = "category") %>% 
                    group_by(disagg, category) %>% 
                    summarise(
                      achieved_total = sum(achieved_total, na.rm=T),
                      progs = n_distinct(programme_id)
                    ) %>% 
                    filter(!is.na(category)) %>% 
                    mutate(category = tolower(category))
                }
              }
              
  y <- y %>% mutate(kpi_id=kpi)
  
  return(y)
  
}
