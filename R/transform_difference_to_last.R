#' differences between last year and this
#'
#' @param x  data results last target
#' @param y  summary_kpi_totals target
#' @param periods  reporting_periods target
#' @param round  logical whether to round down differences

transform_difference_to_last <- function(x, y, round = T, periods){
  
  last <- x %>% 
            rename_at(vars(!contains("kpi")), ~paste0("last_", .))
    
  this <- y %>% 
            select(kpi_id, kpi_title, adj_tpb,
                   intersect(
                     matches(paste0(year(Sys.Date())-1, "|","total")), starts_with("achieved")
                     )
                   ) %>% 
            filter(kpi_id!="ICF KPI 15"&!str_detect(kpi_id, "ICF TA"))

    y <- this %>% 
          left_join(last) %>% 
            mutate(
                   revisions = achieved_total-last_achieved_total-!!sym(paste0("achieved_", tail(periods,1))), 
                   increase_achieved_total = achieved_total - (last_achieved_total + revisions),
                   perc_increase_achieved_total =
                     paste0(format(round((increase_achieved_total/(last_achieved_total + revisions))*100,2), nsmall = 2),"%") %>% str_trim("both")
                    ) 

    if(round==T){
      y <- y %>% mutate(increase_achieved_total = 
                           case_when(
                             increase_achieved_total>10000 ~ round_choose(increase_achieved_total, roundTo=1000),
                             TRUE ~ increase_achieved_total
                           ),
                        revisions = 
                          case_when(
                            revisions > 10000 ~ round_choose(revisions, roundTo=1000),
                            TRUE ~ revisions
                          ),
                        )
    }
    
       y <- y %>% mutate_if(is.numeric, scales::comma)
    
    return(y)
    
  }
