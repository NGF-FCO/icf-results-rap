#' tidy up raw data downloaded from REX
#' 
#' @param x  raw rex data target in pipeline
#' @param remove_progs vector or programmes to remove
#' @param remove_indicators vector of indicators to remove
#' @param years reporting years calculated from get_reporting_years
#' 

tidy_rex <- function(x, remove_progs=NULL, remove_indicators=NULL, years, desnz = progs_desnz){

  y <- 
    x %>% 
    # clean names
    clean_names() %>% 
    # separate programme id column
    separate_wider_regex(activity_title, 
                         patterns = c(programme_id = "^[^:]*",":", 
                                      programme_title = "?:(.*)$")) %>% 
    # separate kpi number and name
    separate_wider_regex(indicator_title, 
                         patterns = c(kpi_id = "^[^:]*",":", 
                                      kpi_title = "?:(.*)$")) %>% 
    # add leading 0 to kpi id for sorting
    mutate(kpi_id = case_when(
      nchar(str_extract(kpi_id, "[+-]?([0-9]*[.])?[0-9]+")) == 1 ~ str_replace(kpi_id, str_extract(kpi_id, "[+-]?([0-9]*[.])?[0-9]+"), paste0("0", str_extract(kpi_id, "[+-]?([0-9]*[.])?[0-9]+"))),
      nchar(str_extract(kpi_id, "[+-]?([0-9]*[.])?[0-9]+")) == 3 ~ str_replace(kpi_id, str_extract(kpi_id, "[+-]?([0-9]*[.])?[0-9]+"), paste0("0", str_extract(kpi_id, "[+-]?([0-9]*[.])?[0-9]+"))),
      TRUE ~ kpi_id
    )) %>% 
    # remove double spaces
    mutate(kpi_title = str_squish(kpi_title)) %>% 
    # remove trailing and leading spaces
    mutate_at(vars(programme_id, programme_title), ~str_trim(.x, side = "both")) %>% 
    mutate_at(vars(programme_id, programme_title), ~str_squish(.x)) %>% 
    # remove strings from shared rex progs that can't be editted in the UI
    mutate(programme_id = str_remove_all(programme_id, " - Northern Uganda")) %>% 
    # add prefix to disagg columns for later grouping/selecting
    rename_at(vars(!1:achieved), ~paste0("disagg_", .x)) %>% 
    # replace NA period_name - will convert unformattable (e.g. char) to 2050 or 2100 so can make 'tpb' consistent at next mutate.
    mutate(period_name = 
                 paste0(format(as.Date(period_start,format="%d/%m/%Y"),format="%Y"),
                        "/",
                        format(as.Date(period_end,format="%d/%m/%Y"),format="%Y")
                        )
           ) %>% 
    # make consistent period_name
    mutate(period_name = 
             case_when(
               nchar(period_name) < 8 ~ str_replace(period_name, "/", "/20"),
               str_detect(period_name, "2100|2050") ~ "tpb",
               TRUE ~ period_name
               )
    ) %>% 
    # dissaggregate ta 2.1 and 2.2 and rename
    mutate(kpi_id = case_when(
      disagg_individuals_or_organisations == "Individuals" & kpi_title== "Number of individuals and organisations supported by ICF Technical Assistance" ~ "ICF TA KPI 02.1",
      disagg_individuals_or_organisations == "Organisations" & kpi_title== "Number of individuals and organisations supported by ICF Technical Assistance" ~ "ICF TA KPI 02.2",
      disagg_individuals_or_organisations == "Unknown" & kpi_title== "Number of individuals and organisations supported by ICF Technical Assistance"~ "ICF TA KPI 02.1",
      is.na(disagg_individuals_or_organisations) & kpi_title== "Number of individuals and organisations supported by ICF Technical Assistance" ~ "ICF TA KPI 02.1",
      TRUE ~ kpi_id    
    )) %>% 
    mutate(kpi_title = case_when(
      kpi_id == "ICF TA KPI 02.1" ~ "Number of individuals supported by ICF Technical Assistance",
      kpi_id == "ICF TA KPI 02.2" ~ "Number of organisations supported by ICF Technical Assistance",
      kpi_id == "ICF TA KPI 05" ~ "Tonnes of Greenhouse Gas Emissions Reduced or Avoided through ICF Technical Assistance", 
      TRUE ~ kpi_title   
    )) %>% 
    # identify DESNZ progs for separate disagg
    mutate(dept = case_when(
      programme_id %in% desnz ~ "desnz",
      str_detect(programme_id, "DefraPO") ~ "defra",
      TRUE ~ "fcdo"
    )) %>% 
    # change CIFs prog titles to merge them
    mutate(
      programme_title = 
        case_when(
          programme_id == "200368" ~ str_remove(programme_title, " -.+"),
          TRUE ~ programme_title
        )
    ) %>% 
    # trim whitespace
    mutate_if(is.character, str_trim, "both") %>% 
    # change prog and kpi titles and ids to factors
    mutate(programme_title = as.factor(programme_title)) %>% 
    mutate(programme_id = as.factor(programme_id)) %>% 
    mutate(kpi_id = as.factor(kpi_id)) %>% 
    mutate(kpi_title = as.factor(kpi_title)) %>% 
    #####---- optionally remove individual programmes or indicators based on ID
    filter(!programme_id %in% remove_progs) %>% 
    filter(!kpi_id %in% remove_indicators)
  
  
  return(y)

}




