#' prepare tables for oecd reporting of private finance figures
#' 
#' @param x  summary main target in pipeline
#' @param fcdo_only logical whether to only include fcdo figures
#' @param dominant_disagg logical to retain all rows or just rows with the disagg that contributes most to total (default)
#' 


table_kpi12_oecd_return <- function(x, fcdo_only = TRUE, dominant_disagg = T){
  
  if(dominant_disagg==T){
     y <- x %>%
           filter(kpi_id=="ICF KPI 12" & 
                    if_any(intersect(starts_with("achieved"),contains("2023")), ~ !is.na(.)) &
                    if_any(intersect(starts_with("achieved"), contains("2023")), ~ . > 0)
                  ) %>%
           select(programme_id, 
                  programme_title, 
                  dept, 
                  `achieved_2022/2023`, 
                  disagg_climate_theme,
                  disagg_leveraging_mechanism_and_role_position, 
                  disagg_origin_of_private_finance_mobilised) %>%
           group_by(programme_id, programme_title) %>% 
           mutate(total_2 = sum(`achieved_2022/2023`)) %>% 
           top_n(1, `achieved_2022/2023`) %>% 
           ungroup()
    
     }else{
    
      y <- x %>%
              filter(kpi_id=="ICF KPI 12" & 
                       !is.na(`achieved_2022/2023`) & 
                       `achieved_2022/2023` > 0) %>% 
              select(programme_id, 
                     programme_title, 
                     dept, 
                     `achieved_2022/2023`, 
                     disagg_climate_theme,
                     disagg_leveraging_mechanism_and_role_position, 
                     disagg_origin_of_private_finance_mobilised) %>%
              group_by(pick(programme_id, programme_title, contains("disagg")), dept) %>% 
              summarise(total = sum(`achieved_2022/2023`)) %>% 
              ungroup() 
     
      }
  
    if(fcdo_only==T){
      y <- filter(y, dept=="fcdo")
    }

  return(y)
  
}



