#' calculate disaggregation summary for any KPI and any disaggregation
#' 
#' @param x  data_raw_desnz_disagg
#' @param y  summary_main or summary_main_grouped
#' 


disagg_desnz <- function(x, y){
 
    kpi <- unique(x$kpi_id)
   
    # get disagg_publication for kpi from summary_main
    disaggs <- y %>% 
                  filter(kpi_id==kpi) %>% 
                  distinct(disagg_publication) %>%
                  pull() %>% 
                  str_split_1(., pattern = ",") %>% 
                  str_trim()
    
    z <- if(kpi=="ICF KPI 01"){
      x %>% 
        filter(kpi_id==kpi & 
                 (str_detect(disagg_direct_indirect_beneficiary, "[Dd]irect") | is.na(disagg_direct_indirect_beneficiary))) %>%
        filter(achieved_total>0) %>% 
        select(programme_id, all_of(disaggs), achieved_total) %>%  
        select(programme_id, where(~n_distinct(.) > 1)) %>% 
        pivot_longer(-c(programme_id,achieved_total),names_to = "disagg", values_to = "category") %>% 
        group_by(disagg, category) %>% 
        summarise(
          achieved_total = sum(achieved_total, na.rm=T),
          progs = n_distinct(programme_id),
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
            progs = n_distinct(programme_id),
          ) %>% 
          filter(!is.na(category)) %>% 
          mutate(category = tolower(category))
        
      }else{
        
        x %>% 
          filter(kpi_id==kpi) %>%
          filter(achieved_total>0) %>% 
          select(programme_id, all_of(disaggs), achieved_total) %>% 
          select(programme_id, where(~n_distinct(.) > 1)) %>% 
          pivot_longer(-c(programme_id, achieved_total), names_to = "disagg", values_to = "category") %>% 
          group_by(disagg, category) %>% 
          summarise(
            achieved_total = sum(achieved_total, na.rm=T),
            progs = n_distinct(programme_id)
          ) %>% 
          filter(!is.na(category)) %>% 
          mutate(category = tolower(category))
      }
    }
    
    z <- z %>% mutate(kpi_id=kpi)
    
    return(z)
    
  }