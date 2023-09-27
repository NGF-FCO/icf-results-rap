#' KPI totals table for publication
#' 
#' @param x  summary_kpi_totals target
#' @param y  summary_disagg_main target
#' @param z  summary_kpi15 target

table_kpi_totals <- function(x, y, z, ta1_disaggs=TRUE) {
    
  fifteen_top <- z %>% 
                    filter(scale=="new") %>% 
                    mutate(achieved = case_when(
                      achieved=="4" ~ "5",
                      achieved!="4" & achieved!="5" ~ "1",
                      TRUE ~ achieved
                    )
                    )%>% 
                    group_by(achieved) %>% 
                    summarise(cat = n()) %>% 
                    mutate(perc_cat = paste0(format(round((cat/sum(cat))*100,2), nsmall=2), "%")) %>% 
                    filter(achieved=="5") %>% 
                    pull(perc_cat)
  
  if(ta1_disaggs==TRUE){
    
  t1 <- y %>% 
            filter(kpi_id=="ICF TA KPI 01" & disagg=="disagg_country" & category!="unspecified") %>% 
            group_by(kpi_id) %>% 
            count(name = "achieved_total") %>% 
            mutate(achieved_total = as.character(achieved_total),
                   units = "Countries",
                   achieved_total = paste0(achieved_total, " ", units),
                   kpi_title = "Number of Countries Supported by ICF Technical Assistance")
  
  fifteen_top <- z %>% 
                      filter(scale=="new") %>% 
                      mutate(achieved = case_when(
                        achieved=="4" ~ "5",
                        achieved!="4" & achieved!="5" ~ "1",
                        TRUE ~ achieved
                      )
                      )%>% 
                      group_by(achieved) %>% 
                      summarise(cat = n()) %>% 
                      mutate(perc_cat = paste0(format(round((cat/sum(cat))*100,2), nsmall=2), "%")) %>% 
                      filter(achieved=="5") %>% 
                      pull(perc_cat)
                    
  y <- x %>% 
            # remove TA1 kpi 
            filter(kpi_id!="ICF TA KPI 01") %>% 
            select(kpi_id, kpi_title, achieved_total, units) %>% 
            mutate(achieved_total = 
                     case_when(
                       achieved_total > 10000 ~ round_choose(achieved_total, roundTo=1000),
                       TRUE ~ achieved_total
                     ),
                   achieved_total  = scales::comma(achieved_total),
                   achieved_total = 
                     case_when(
                       kpi_id == "ICF KPI 15" ~ paste0(fifteen_top, " programmes scored a 4 or 5"),
                       units=="£" ~ paste(units, achieved_total, sep = " "), 
                       TRUE ~ paste(achieved_total, units, sep = " ")
                     )
            ) %>%
            # add TA1 back in 
            bind_rows(., t1) %>% 
            arrange(kpi_id) %>% 
            mutate(kpi_id = str_remove_all(kpi_id, "ICF ")) %>% 
            select(-units) %>% 
            rename("KPI Number" = kpi_id) %>% 
            rename("KPI Title" = kpi_title) %>% 
            rename("Achieved Total" = achieved_total)
  
  }else{
  
    fifteen_top <- summary_kpi15 %>% 
                      filter(scale=="new") %>% 
                      mutate(achieved = case_when(
                        achieved=="4" ~ "5",
                        achieved!="4" & achieved!="5" ~ "1",
                        TRUE ~ achieved
                      )
                      )%>% 
                      group_by(achieved) %>% 
                      summarise(cat = n()) %>% 
                      mutate(perc_cat = paste0(format(round((cat/sum(cat))*100,2), nsmall=2), "%")) %>% 
                      filter(achieved=="5") %>% 
                      pull(perc_cat)
    
  y <- x %>% 
          select(kpi_id, kpi_title, achieved_total, units) %>% 
          arrange(kpi_id) %>% 
          mutate(achieved_total = 
                   case_when(
                              achieved_total > 10000 ~ round_choose(achieved_total, roundTo=1000),
                              TRUE ~ achieved_total
                            ),
                 achieved_total  = scales::comma(achieved_total),
                 achieved_total = 
                   case_when(
                              kpi_id == "ICF KPI 15" ~ paste0(fifteen_top, " programmes scored a 4 or 5"),
                              units=="£" ~ paste(units, achieved_total, sep = " "), 
                              TRUE ~ paste(achieved_total, units, sep = " ")
                            )
                 ) %>% 
          mutate(kpi_id = str_remove_all(kpi_id, "ICF ")) %>% 
          select(-units) %>% 
          rename("KPI Number" = kpi_id) %>% 
          rename("KPI Title" = kpi_title) %>% 
          rename("Achieved Total" = achieved_total)
  }
  
  return(y)
  
}
