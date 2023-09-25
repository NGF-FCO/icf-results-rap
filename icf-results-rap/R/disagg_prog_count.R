#' recalculating programme counts for disaggs
#' 
#' @param x summary_main
#' @param y data_raw_desnz_disagg
#' @param kpi_totals summary_kpi_progs


disagg_prog_count <- function(x, y, kpi_totals) {
  
    totals <- kpi_totals
    
    disaggs <- x %>% 
                select(kpi_id, disagg_publication) %>% 
                filter(!is.na(disagg_publication)) %>% 
                distinct()
         
                
    fcdo <- x %>% 
                filter(achieved_total>0) %>% 
                select(programme_id, kpi_id, achieved_total, contains("disagg"), -disagg_publication) %>% 
                pivot_longer(-c(programme_id, kpi_id, achieved_total), names_to = "disagg", values_to = "category") %>%
                mutate(category=str_to_lower(category)) %>% 
                filter(!is.na("category") & category!="cannot feasibly be collected" & category!="unknown" & category!="other") %>% 
                group_by(kpi_id, disagg) %>% 
                summarise(progs = n_distinct(programme_id))
    
    
    desnz <- y %>% 
                filter(achieved_total>0) %>% 
                select(programme_id, kpi_id, achieved_total, contains("disagg"), -disagg_reported) %>% 
                pivot_longer(-c(programme_id, kpi_id, achieved_total), names_to = "disagg", values_to = "category") %>%
                mutate(category=str_to_lower(category)) %>% 
                filter(!is.na("category") & category!="cannot feasibly be collected" & category!="unknown" & category!="other") %>% 
                group_by(kpi_id, disagg) %>% 
                summarise(progs = n_distinct(programme_id))
    
    z <- bind_rows(fcdo, desnz) %>% 
            left_join(totals) %>%
            left_join(disaggs) %>% 
            mutate(disagg_publication = str_replace_all(disagg_publication, ", ", "|")) %>% 
            filter(str_detect(disagg, disagg_publication)) %>% 
            select(-kpi_title, -disagg_publication) %>% 
            group_by(kpi_id, disagg) %>% 
            reframe(progs = sum(progs),
                    prop = progs/n,
                    perc = paste0(round_choose((progs/n)*100, 1), "%")
                   ) %>% 
            distinct(kpi_id, disagg, .keep_all = T) 
            
  return(z)
    
}
