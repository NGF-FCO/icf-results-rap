#' tidy up raw data downloaded from REX
#' 
#' @param x  raw rex data target in pipeline
#' @param remove_progs vector or programmes to remove
#' @param remove_indicators vector of indicators to remove
#' @param year reporting years calculated from get_reporting_years
#' 

tidy_rex_last <- function(x, remove_progs=NULL, remove_indicators=NULL, years, desnz = progs_desnz){

  y <- 
    x %>% 
    # clean names
    clean_names() %>% 
    # filter out test rows
    filter(!str_detect(activity_title, "(?i)test")) %>%
    # separate programme id column
    separate_wider_regex(activity_title, patterns = c(programme_id = "^[^:]*",":", programme_title = "?:(.*)$")) %>%
    # separate kpi number and name
    separate_wider_regex(indicator_title, patterns = c(kpi_id = "^[^:]*",":", kpi_title = "?:(.*)$")) %>% 
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
    # add prefix to disagg columns for later grouping/selecting
    rename_at(vars(sex:ncol(.)), ~ paste0("disagg_", .x)) %>% 
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
    # identify DESNZ progs for separate disagg
    mutate(dept = case_when(
      programme_id %in% desnz ~ "desnz",
      str_detect(programme_id, "DefraPO") ~ "defra",
      TRUE ~ "fcdo"
    )) %>% 
    # trim whitespace
    mutate_if(is.character, str_trim, "both") %>% 
    # change prog and kpi titles and ids to factors
    mutate(programme_title = as.factor(programme_title)) %>% 
    mutate(programme_id = as.factor(programme_id)) %>% 
    mutate(kpi_id = as.factor(kpi_id)) %>% 
    mutate(kpi_title = as.factor(kpi_title)) %>% 
    # add prefix to diagg cols
    #####---- optionally remove individual programmes or indicators based on ID
    filter(!programme_id %in% remove_progs) %>% 
    filter(!kpi_id %in% remove_indicators)
  
  return(y)

}




