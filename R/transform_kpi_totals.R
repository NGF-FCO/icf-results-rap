#' KPI totals (mean for KPI15, unique countries per year for TAKPI01)
#' 
#' @param x  summary_main target
#' @param years  reporting_years target
#' @param periods  reporting_periods target
#' @param ta01  summary_ta01 target

transform_kpi_totals <- function(x, years, ta01, periods) {
  
  t1 <- ta01 %>%
    filter(period_name %in% periods) %>% 
    group_by(period_name) %>% 
    count() %>% 
    #mutate(id = row_number()) %>% 
    pivot_wider(names_from = "period_name", 
                names_prefix = "achieved_", 
                values_from = "n", 
    ) %>% 
    mutate(achieved_total = 
             unlist(ta01, use.names=FALSE) %>% 
             unique() %>% 
             str_subset(.,"/", negate=T) %>% 
             length()
    ) %>% 
    mutate(tpb=achieved_total) %>% 
    mutate(adj_tpb=achieved_total) %>% 
    mutate(kpi_id = "ICF TA KPI 01") %>% 
    mutate(kpi_title = "Number of Countries Supported by ICF Technical Assistance") %>% 
    arrange(kpi_id)
  
  kpi15 <- x %>% 
              filter(kpi_id == "ICF KPI 15") %>% 
              select(kpi_id, kpi_title, intersect(matches(years), matches("achieved|planned")), matches("tpb"), matches("achieved_total")) %>%
              group_by(kpi_id, kpi_title) %>%
              # for all instances of KPI 15 find the mean in each col so all KPI15 rows are the same
              mutate_if(is.numeric, ~replace(.x, 
                                             kpi_id == 'ICF KPI 15', 
                                             mean(.x[kpi_id == 'ICF KPI 15'], na.rm=T))) %>%
              distinct()
  
  
  z <- x %>%
        filter(disagg_direct_indirect_beneficiary == "Direct" | is.na(disagg_direct_indirect_beneficiary)) %>%
        filter(kpi_id != "ICF TA KPI 01" & kpi_id != "ICF KPI 15") %>% 
        select(kpi_id, kpi_title, intersect(matches(years), matches("achieved|planned")), matches("tpb"), matches("achieved_total")) %>%
        group_by(kpi_id, kpi_title) %>%
        reframe(across(where(is.numeric), ~sum(.x, na.rm = T))) %>% 
        ungroup() %>% 
        mutate(revised_achieved_total_last = 
                 rowSums(
                   across(
                     intersect(
                       starts_with("achieved"), matches(glue::glue_collapse(periods[-length(periods)],"|"))
                       )
                     ), na.rm=T)
        ) %>% 
        rowwise() %>% 
        mutate(perc_increase_achieved_total = (`achieved_2022/2023`/revised_achieved_total_last)*100) %>% 
        ungroup() %>% 
      # add in TA KPI 1 and KPI15 info
      bind_rows(.,t1) %>% 
        bind_rows(., kpi15) %>% 
        mutate_if(is.numeric, round, 2) %>% 
        mutate(units = case_when(
          kpi_id %in% c("ICF KPI 01", "ICF KPI 02.1", "ICF KPI 04", "ICF TA KPI 02.1") ~ "People",
          kpi_id %in% c("ICF KPI 02.2") ~ "Institutions",
          kpi_id %in% c("ICF TA KPI 02.2") ~ "Organisations",
          kpi_id %in% c("ICF TA KPI 03") ~ "Policies",
          kpi_id %in% c("ICF KPI 06", "ICF TA KPI 05") ~ "Tonnes of CO2 (tCO2e)",
          kpi_id %in% c("ICF KPI 07") ~ "Megawatts (MW)",
          kpi_id %in% c("ICF KPI 08", "ICF KPI 17") ~ "Hectares",
          kpi_id %in% c("ICF KPI 11", "ICF KPI 12", "ICF KPI 10") ~ "GBP (£)",
          kpi_id %in% c("ICF KPI 15") ~ "[Scorecard breakdown in KPI 15 section]",
          kpi_id %in% c("ICF TA KPI 01") ~ "Countries",
          TRUE ~ NA
        )) %>% 
        arrange(kpi_id)

  return(z)             

}