#' Runs some data validation checks
#' print results and saves to cache
#' 
#' checks achieved data haven't accidentally been entered in future years.
#' checks kpi15 scores for current year are valid.
#' checks for duplicate observations.
#' checks achieved > planned. Planned shold be updated to at least equal achieved. 
#' checks for cases where tpb is given but nothing for achieved or planned future years. 
#' 
#' @param rp reporting_periods target
#' @param ... need to supply rex and summary_main in whatever order. will error if not supplied. 


data_checks <- function( rp = reporting_periods, ...){

args <- list(...)

dots <- sapply(match.call(expand.dots = FALSE)$..., deparse)

stopifnot(str_detect(dots, "rex|summary_main"))

r <- args[str_detect(dots, "rex")] %>% .[[1]]

sm <- args[str_detect(dots, "summary_main")] %>% .[[1]]

# check achieved data in future years are 0
  # test data
    future_years <- r %>%
                        select("programme_id","period_name","achieved") %>%
                        dplyr::filter(!period_name %in% c(rp, "tpb") & achieved>0) %>%
                        group_by(programme_id) %>%
                        summarise(n = n_distinct(programme_id)) %>%
                        pull(n)

  # test
    future_years_results <- ifelse(length(future_years) == 0, "test passed", "test failed - check programmes with achieved figures in future years")
    names(future_years_results) <- "future_years"


# check kpi 15 scores are one of 1,2,3,4,5 for current year
  # test_data
  kpi15 <- r %>%
                select("programme_id","kpi_id","period_name","achieved") %>%
                dplyr::filter(kpi_id=="ICF KPI 15" & achieved!=0 & period_name %in% c(tail(rp,1))) %>%
                group_by(programme_id, achieved) %>%
                pull(achieved) %>%
                unique() %>% 
                sort() %>% 
                paste(collapse=",")
  # test
  kpi15_results <- ifelse(kpi15=="1,2,3,4,5", "test passed", "test failed - check KPI15 scores")
  names(kpi15_results) <- "kpi15_scores"    



# check for duplicate data entries in rex
  dupes <- r %>% 
            group_by(programme_id, kpi_id, pick(contains("disagg_"))) %>% 
            duplicated() %>%
            unique()
  
  dupes_results <- ifelse(dupes==FALSE, "test passed", "test failed - check for duplicate observations")
  names(dupes_results) <- "dupes" 

# check achieved figures > planned for current year
  x <-  sm %>%
          select(
            intersect(matches("achieved|planned"),
                      contains(tail(rp,1))
                      )
                ) %>%
          filter_all(any_vars(. != 0))  %>%
          mutate(diff = .[[1]]>.[[2]]) %>%
          pull(diff) %>%
          unique()

  achieved_greater_than_planned_results <- ifelse(FALSE %in% x, "test failed - data contains observations where planned have not been updated to equal achieved", "test passed")
  names(achieved_greater_than_planned_results) <- "achieved_greater_than_planned" 



# check tpb where achieved_total is 0
  tpb_no_achieved <-  sm %>%
                          select(programme_id,programme_title, kpi_id, achieved_total, planned_future_years, tpb) %>%
                          group_by(programme_id, programme_title, kpi_id) %>%
                          summarise_if(is.numeric, sum, na.rm=T) %>%
                          filter(achieved_total==0 & tpb>0 & planned_future_years==0) %>% 
                          nrow()
          
  
  tpb_no_achieved_results <- ifelse(tpb_no_achieved==0, "test passed", "test failed - some observations have no achieved or planned results but tpb. please check")
  names(tpb_no_achieved_results) <- "tpb_no_achieved" 

  print(future_years_results)  
  print(kpi15_results)
  print(dupes_results)
  print(achieved_greater_than_planned_results )
  print(tpb_no_achieved_results)
  
y <- rbind(future_years_results, kpi15_results, dupes_results, achieved_greater_than_planned_results , tpb_no_achieved_results) %>% 
      as_tibble(rownames = "rowname") %>% 
      rename(pass_fail=future_years) %>% 
      rename(test=rowname) %>% 
      mutate(test = str_remove_all(test, "_results"))

return(y)

}
