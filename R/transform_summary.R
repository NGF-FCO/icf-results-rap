#' tidy up raw data downloaded from REX
#' 
#' @param x  rex data target in pipeline
#' @param years calculated from get_reporting_years
#' 

transform_summary <- function(x, years){

 y <- x %>% 
   group_by(pick(period_name, programme_title, programme_id, kpi_title, kpi_id, contains("disagg"), dept)) %>% 
   summarise(achieved = sum(achieved, na.rm = T), planned = sum(planned, na.rm = T)) %>% 
   pivot_wider(names_from = c(period_name), values_from = c(achieved,planned), values_fn = sum) %>%
   # remove achieved future years
   select(!(starts_with("achieved_") & -matches(glue::glue_collapse(c(years),"|")))) %>% 
   # sum across achieved totals
   mutate(achieved_total = 
            case_when(
              kpi_id!="ICF KPI 15" ~ rowSums(
                across(
                  intersect(matches(glue::glue_collapse(c(years),"|")), starts_with("achieved_"))
                ), na.rm = T),
              TRUE ~ do.call(pmax,  
                c(across(intersect(starts_with("achieved_"), matches("/"))), na.rm=T)
                
              )
            )
   ) %>% 
   # sum across total planned years (excludes planned_tpb) but max for kpi15
   mutate(planned_total_years = 
            case_when(
              kpi_id!="ICF KPI 15" ~ rowSums(
                across(
                  intersect(matches("/"), starts_with("planned_"))
                ), na.rm = T),
              TRUE ~ do.call(pmax, 
                             c(across(intersect(starts_with("planned_"), matches("/"))), na.rm=T)
                             )
              )
   ) %>% 
   # sum across planned current years 
   mutate(planned_current_years = 
     case_when(
       kpi_id!="ICF KPI 15" ~ rowSums(
         across(
           intersect(matches(glue::glue_collapse(c(years),"|")), starts_with("planned_"))
         ), na.rm = T),
       TRUE ~ do.call(pmax, 
                      c(across(intersect(starts_with("planned_"), matches(glue::glue_collapse(c(years),"|")))), na.rm=T)
                      )
       )
   ) %>% 
   # sum across future years (excludes planned_tpb, planned_current_years, planned_future_years, and the current reporting period years)
   mutate(planned_future_years =
     case_when(
       kpi_id!="ICF KPI 15" ~ rowSums(
         across(
           starts_with("planned_") & -matches(c(paste0("tpb|years|", glue::glue_collapse(c(years),"|"))))
         )
         , na.rm = T),
       TRUE ~ do.call(pmax, 
                      c(across(
                        starts_with("planned_") & -matches(c(paste0("tpb|years|",glue::glue_collapse(c(years),"|"))))
                              ), na.rm=T
                      )
                      )
       )
   ) %>% 
   # rename planned_tpb
   rename("tpb" = planned_tpb) %>%
   # calc adjusted total planned years (where planned current < achieved_total) as achieved total plus future years
   mutate(tpb = replace_na(tpb, 0)) %>% 
   mutate(adj_total_planned_years = 
            case_when(
              planned_total_years < achieved_total + planned_future_years ~ achieved_total + planned_future_years,
              TRUE ~ planned_total_years
            )
   ) %>% 
   # calc adj tpb as adj total planned years where adj total planned is less than tpb 
   mutate(adj_tpb = 
            case_when(
              tpb < adj_total_planned_years ~ adj_total_planned_years,
              TRUE ~ tpb
            )
   ) %>% 
   
   mutate(disagg_publication = case_when(
     kpi_id == "ICF KPI 01" ~ "disagg_sex, disagg_age, disagg_disability, disagg_geography",
     kpi_id == "ICF KPI 02.1" ~ "disagg_sex, disagg_age, disagg_disability, disagg_geography, disagg_clean_energy",
     kpi_id == "ICF KPI 04" ~ "disagg_sex, disagg_disability, disagg_geography",
     kpi_id == "ICF KPI 06" ~ "disagg_sector",
     kpi_id == "ICF KPI 07" ~ "disagg_geography, disagg_technology_type, disagg_on_off_grid",
     kpi_id == "ICF KPI 11" ~ "disagg_climate_theme, disagg_origin_of_public_finance_mobilised",
     kpi_id == "ICF KPI 12" ~ "disagg_climate_theme, disagg_origin_of_private_finance_mobilised, disagg_leveraging_mechanism_and_role_position",
     kpi_id == "ICF KPI 15" ~ "disagg_sector",
     kpi_id == "ICF KPI 17" ~  "disagg_ecosystem_type, disagg_sustainable_management_practice, disagg_sustainable_management_theme, disagg_country", 
     kpi_id == "ICF TA KPI 01" ~ "disagg_country, disagg_type_of_technical_assistance, disagg_sector",
     str_detect(kpi_id, "ICF TA KPI 02|ICF TA KPI 03|ICF TA KPI 05") ~ "disagg_type_of_technical_assistance, disagg_sector",
     TRUE ~ NA
    )
   ) %>% 
   
   ungroup()   

  return(y)

}

