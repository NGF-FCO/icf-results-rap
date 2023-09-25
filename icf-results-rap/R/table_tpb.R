#' total programme benefits table for annex
#' 
#' @param x  summary_main target


table_tpb <- function(x){
  
  y <- x %>% 
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
        filter(disagg_direct_indirect_beneficiary == "Direct" | is.na(disagg_direct_indirect_beneficiary)) %>% 
        group_by(kpi_id, kpi_title, units) %>% 
        summarise(tpb = sum(adj_tpb, na.rm=T)) %>% 
        mutate(tpb = case_when(
          tpb > 10000 ~ round_choose(tpb, roundTo=1000),
          TRUE ~ tpb
        ),
        tpb = scales::comma(tpb),
        tpb = paste0(tpb, " ", units),
        tpb = case_when(
          kpi_id == "ICF KPI 15" ~ "Not Applicable",
          kpi_id == "ICF TA KPI 01" ~ "Not Applicable",
          TRUE ~ tpb
        )) %>% 
        select(-units) %>% 
        rename("KPI Number" = kpi_id,
               "KPI Title" = kpi_title,
               "Expected Total Programme Benefits" = tpb) 
        
  return(y)
  
}
