#' cumsum long format of results by kpi by year
#' @param x summary_main target
#' @param kpi values we have. Character (must be quoted)
#' @param years reporting_years target


transform_cumsum <- function(x, kpi, years){
  
  if(missing(kpi)){
    
    x %>% 
      filter(disagg_direct_indirect_beneficiary == "Direct" | is.na(disagg_direct_indirect_beneficiary)) %>% 
      filter(kpi_id!="ICF KPI 15" & kpi_id != "ICF TA KPI 01") %>% 
      select(kpi_id, kpi_title, 
             intersect(starts_with("achieved"), matches(years))) %>% 
      group_by(kpi_id, kpi_title) %>% 
      reframe(across(where(is.numeric), ~sum(.x, na.rm = T))) %>% 
      pivot_longer(-c(kpi_id, kpi_title), 
                   names_to = "year", 
                   values_to = "achieved", 
                   names_prefix = "achieved_") %>% 
      mutate(year = str_extract(year, "\\w+$")) %>% 
      group_by(kpi_id, kpi_title) %>% 
      mutate(achieved_cumulative = cumsum(achieved)) %>% 
      arrange(kpi_id, year) %>%
      mutate(units = case_when(
        kpi_id %in% c("ICF KPI 01", "ICF KPI 02.1", "ICF KPI 04", "ICF TA KPI 02.1") ~ "People",
        kpi_id %in% c("ICF KPI 02.2") ~ "Institutions",
        kpi_id %in% c("ICF TA KPI 02.2") ~ "Organisations",
        kpi_id %in% c("ICF TA KPI 03") ~ "Policies",
        kpi_id %in% c("ICF KPI 06", "ICF TA KPI 05") ~ "Tonnes of CO2 (tCO2e)",
        kpi_id %in% c("ICF KPI 07") ~ "Megawatts",
        kpi_id %in% c("ICF KPI 08", "ICF KPI 17") ~ "Hectares",
        kpi_id %in% c("ICF KPI 11", "ICF KPI 12", "ICF KPI 10") ~ "GBP (£)",
        kpi_id %in% c("ICF KPI 15") ~ "[Scorecard breakdown in KPI 15 section]",
        kpi_id %in% c("ICF TA KPI 01") ~ "Countries",
        TRUE ~ NA
      )) %>% 
      mutate(kpi_title = str_remove_all(kpi_title, "as a result of ICF"),
             kpi_title = case_when(
               !str_detect(kpi_title, "people|individuals|institutions") ~ paste0(kpi_title, " (", units, ")"),
               TRUE ~ kpi_title
             )
      ) %>% 
      ungroup()
    
  } else {
    
    x %>% 
      select(kpi_id, kpi_title,
             intersect(starts_with("achieved"), matches(years))) %>% 
      filter(kpi_id!="ICF KPI 15" & kpi_id != "ICF TA KPI 01") %>% 
      pivot_longer(-c(kpi_id, kpi_title), 
                   names_to = "year", 
                   values_to = "achieved", 
                   names_prefix = "achieved_") %>% 
      mutate(year = str_extract(year, "\\w+$")) %>% 
      group_by(kpi_id, kpi_title, year) %>% 
      summarise(achieved = sum(achieved, na.rm=T)) %>% 
      mutate(achieved_cumulative = cumsum(achieved)) %>% 
      arrange(kpi_id, year) %>%
      mutate(units = case_when(
        kpi_id %in% c("ICF KPI 01", "ICF KPI 02.1", "ICF KPI 04", "ICF TA KPI 02.1") ~ "People",
        kpi_id %in% c("ICF KPI 02.2") ~ "Institutions",
        kpi_id %in% c("ICF TA KPI 02.2") ~ "Organisations",
        kpi_id %in% c("ICF TA KPI 03") ~ "Policies",
        kpi_id %in% c("ICF KPI 06", "ICF TA KPI 05") ~ "Tonnes of CO2 (tCO2e)",
        kpi_id %in% c("ICF KPI 07") ~ "Megawatts",
        kpi_id %in% c("ICF KPI 08", "ICF KPI 17") ~ "Hectares",
        kpi_id %in% c("ICF KPI 11", "ICF KPI 12", "ICF KPI 10") ~ "GBP (£)",
        kpi_id %in% c("ICF KPI 15") ~ "[Scorecard breakdown in KPI 15 section]",
        kpi_id %in% c("ICF TA KPI 01") ~ "Countries",
        TRUE ~ NA
      )) %>% 
      mutate(kpi_title = str_remove_all(kpi_title, "as a result of ICF"),
             kpi_title = case_when(
               !str_detect(kpi_title, "people|individuals|institutions") ~ paste0(kpi_title, " (", units, ")"),
               TRUE ~ kpi_title
               )
             ) %>% 
      filter(kpi_id %in% kpi) %>% 
      ungroup()
    
  }
  
}


