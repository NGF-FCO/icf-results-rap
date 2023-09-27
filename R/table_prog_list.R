#' calculate disaggregation summary for KPIs
#' 
#' @param x  summary main
#'

table_prog_list <- function(x){
  
  y <- x %>% 
          filter(achieved_total>0) %>% 
          select(programme_id, programme_title) %>% 
          arrange(desc(programme_id)) %>% 
          distinct() %>% 
          rowwise() %>% 
          mutate(programme_id = case_when(
                      str_detect(programme_id, "BEIS") & !str_detect(programme_title, "\\(") ~ 
                        paste0(
                          str_remove(programme_id, "BEIS"),
                          "-",
                          str_extract_all(programme_title, "[A-Z]+") %>% unlist() %>% paste0(collapse = "")
                        ),
                      str_detect(programme_id, "BEIS") ~ 
                        paste0(
                          str_remove(programme_id, "BEIS"),
                          "-",
                          str_extract(programme_title, "(?<=\\().+?(?=\\))")[[1]]
                          
                        ),
                      TRUE ~ programme_id       
                    ),
                programme_id = str_remove_all(programme_id, " ")
            ) %>% 
          rename("Programme ID"=programme_id,
                 "Programme Title"=programme_title) %>% 
          ungroup()
          
  return(y)
  
}