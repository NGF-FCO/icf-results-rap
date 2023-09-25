library(testthat)


# check users haven't accidentally put achieved figures in future years
test_that("future_years",{
  expect_true(tar_read(rex) %>% 
                select("programme_id","programme_title","kpi","period_name","achieved") %>% 
                filter(period_name %in% c("2023/2024","2024/2025","2025/2026")) %>% 
                summarise(n = sum(achieved))==0)
}
)


