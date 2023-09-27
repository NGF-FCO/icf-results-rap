library(testthat)


# check achieved figures aren't accidentally input to future years
test_that("future_years",{
  x <- tar_read(rex) %>% 
                select("programme_id","period_name","achieved") %>% 
                dplyr::filter(!period_name %in% c(tar_read(reporting_periods), "tpb") & achieved>0) %>% 
                group_by(programme_id) %>% 
                summarise(n = n_distinct(programme_id))
  expect_equal(object = x$n, expected = 0)   
}
)

# check kpi15 scores are valid
test_that("kpi15",{
  x <- tar_read(rex) %>% 
                select("programme_id","kpi_id","period_name","achieved") %>% 
                dplyr::filter(kpi_id=="ICF KPI 15" & achieved!=0 & period_name %in% c(tail(tar_read(reporting_periods),1))) %>% 
                group_by(programme_id, achieved) %>% 
                pull(achieved) %>% 
                unique() %>% sort()
  expect_equal(object = x, expected = c(1,2,3,4,5))
}
)


# check for duplicate data entries in rex
test_that("duplicates",{
  x <- tar_read(rex) %>% 
          duplicated() %>% 
          unique()
  expect_false(object = x)
}
)


# check achieved figures > planned
test_that("achieved_greater_than_planned",{
  x <- tar_read(summary_main) %>% 
          select(
            intersect(matches("achieved|planned"), 
                      contains(tail(tar_read(reporting_periods),1))
                      )
          ) %>% 
    filter_all(any_vars(. != 0))  %>% 
    mutate(diff = .[[1]]>.[[2]]) %>% 
    pull(diff) %>% 
    unique()
  
  expect_in(object = x, expected = c("TRUE"))
}
)

