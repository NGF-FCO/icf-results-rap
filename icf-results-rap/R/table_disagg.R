#' tables for publication
#' 
#' @param x  summary_disagg_main target
#' 

table_disagg <- function(x){

  y <- x %>% 
        mutate(disagg = str_remove_all(disagg, "disagg_")) %>% 
        mutate(achieved_total = case_when(
          achieved_total>=10000 ~ comma(round_choose(achieved_total, 1000),1),
          achieved_total<10000 & achieved_total>1000 ~ comma(round_choose(achieved_total, 100),1),
          kpi_id %in% c("ICF KPI 01", "ICF KPI 02.1", "ICF KPI 04", "ICF TA KPI 02.1") & achieved_total < 20 ~ "<20",
          TRUE ~ comma(achieved_total,1)
        )
        ) %>% 
        mutate(progs = case_when(
          progs<5 ~ "<5",
          TRUE ~ as.character(progs)
          )
        ) %>%
        mutate(category = forcats::fct_relevel(category, 
                                               "child (age 0-14)",
                                               "youth (age 15-24)",
                                               "adult (age 25-64)", 
                                               "elder (age 65+)","rural", 
                                               "urban", 
                                               "disabled", 
                                               "not disabled", 
                                               "female", 
                                               "male" )) %>% 
        mutate(category = forcats::fct_relevel(category, "unspecified", after = Inf)) %>% 
        arrange(kpi_id, disagg, category) %>% 
        mutate(disagg = str_replace_all(disagg, "_", " ")) %>% 
        mutate_at(c("disagg", "category"), str_to_title) %>% 
        mutate(category = str_replace_all(category, "\\(Csp\\)", "\\(CSP\\)"),
               category = str_replace_all(category,"\\(Pv\\)","\\(PV\\)"),
               category = str_replace_all(category,"Spvs","SPVs"),
               category = str_replace_all(category,"oecd","OECD"),
               )%>% 
        rename("Disaggregation" = category,
               "Total Achieved" = achieved_total,
               "Number of Programmes" = progs) %>% 
        select(-perc_without_unspecified, -perc_with_unspecified, -perc_text_with_unspecified, -perc_text_without_unspecified) 
  
    return(y)
  
  }


  


    
 
   
 