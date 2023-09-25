#' merging desnz and rex and recalculating "unspecificied"
#' 
#' @param rex summary_disagg_rex
#' @param desnz summary_disagg_desnz
#' @param kpi_totals summary_kpi_totals
#' 


disagg_join <- function(rex, desnz, kpi_totals) {

  totals <- kpi_totals %>% select(kpi_id, achieved_total)

  x <- bind_rows(rex, desnz) %>%
        mutate(category = case_when(
                              category == "cannot feasibly be collected" ~ "unspecified",
                              category == "unknown" ~ "unspecified",
                              str_detect(category, "other") ~ "unspecified",
                              TRUE ~ category
                              )
              ) %>% 
        mutate(category = case_when(
          str_detect(category,"yanmar") ~ "myanmar", 
          TRUE ~ category
        )
        ) %>% 
        group_by(disagg, category, kpi_id) %>% 
        summarise_all(., sum, na.rm = TRUE) %>% 
        arrange(kpi_id) %>% 
        ungroup()
  
  progs <- x %>% select(-achieved_total)
  
  y <-  x %>%
          pivot_wider(id_cols = -c(progs), values_from = achieved_total, names_from = category) %>%
          unnest(cols = everything() ) %>% 
          group_by(disagg, kpi_id) %>% 
          left_join(totals) %>% 
          mutate(unspecified = 
                   achieved_total -
                   rowSums(across(!matches("unspecified|achieved_total|progs")), na.rm = T)
                 ) %>% 
          select(-achieved_total) %>% 
          pivot_longer(-c(disagg, kpi_id), names_to = "category", values_to = "achieved_total", values_drop_na = T) %>% 
          distinct(.keep_all = T) %>%
          left_join(progs) %>% 
          ungroup() %>% 
          group_by(disagg, kpi_id) %>% 
          mutate(perc_without_unspecified = case_when(
                                                category!="unspecified" ~ as.numeric(format(round(100 * (achieved_total/sum(achieved_total[!category=="unspecified"])),2), nsmall=2)),
                                                TRUE ~ NA
                                                    ),
                 perc_with_unspecified = as.numeric(format(round(100 * (achieved_total/sum(achieved_total)),2), nsmall=2)),
                 perc_text_without_unspecified = case_when(perc_without_unspecified >=1 ~ paste0(as.character(perc_without_unspecified),"%"),
                                                           category=="unspecified" ~ NA,
                                                           TRUE ~ "<1%"
                                                          ),
                 perc_text_with_unspecified = case_when(category=="unspecified" ~ paste0(format(round(100 - perc_with_unspecified, 2),nsmall=2),"%"),
                                                        TRUE ~ NA
                                                        )
                ) %>% 
          mutate_if(is.character, str_trim, "both") %>% 
          arrange(kpi_id, disagg, category) %>% 
          ungroup()
  
  return(y)

}



