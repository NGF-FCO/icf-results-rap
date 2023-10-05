## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)
source("tests/test.R")

options(scipen = 999,
        tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

sysfonts::font_add(here::here("font/GDSTransportWebsite-Bold.ttf"), family="gds_bold")
sysfonts::font_add(here::here("font/GDSTransportWebsite.ttf"), family="gds")
sysfonts::font_add(here::here("font/Roboto-Medium.ttf"), family="roboto_med")
sysfonts::font_add(here::here("font/Roboto-Regular.ttf"), family="roboto")

showtext::showtext_auto()
showtext::showtext_opts(dpi=300)

ifelse(!dir.exists(here("images")), dir.create(here("images")), FALSE)
ifelse(!dir.exists(here("tables")), dir.create(here("tables")), FALSE)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

# 1. Data ----
  
  # raw data from rex, last years download, desnz disaggregations, list of desnz progs 
  tar_file_read(data_raw, here::here("data", "exported-data.csv"), read_csv(file = !!.x, col_types = cols())),
  tar_file_read(data_raw_last, here::here("data", "exported-data_2022.csv"), read_csv(file = !!.x, col_types = cols())),
  tar_file_read(data_raw_desnz_disagg, here::here("data", "desnz_disaggregations.csv"), read_csv(file = !!.x, col_types = cols())),
  tar_file_read(progs_desnz, here::here("data", "desnz_progs.csv"), read_csv(file = !!.x, col_types = cols()) %>% pull(1)),
  # published headline figures from previous year
  tar_file_read(data_results_last, here::here("data", "2022_published-icf-results_kpi-totals-tables.csv"), read_csv(file = !!.x, col_types = cols())),
  tar_file_read(kpi15_scores, here::here("data", "kpi15_amended_scores.csv"), read_csv(file = !!.x, col_types = cols())),
  # make periods and years for current reporting year
  tar_target(reporting_years, get_reporting_years()),
  tar_target(reporting_years_last, reporting_years %>% head(-1)),
  tar_target(reporting_periods, get_reporting_periods(years = reporting_years)),
  tar_target(reporting_periods_last, reporting_periods %>% head(-1)),


# 2. Tidy Data ----

  # tidy raw data and optionally remove programmes or kpis from analysis - see tidy_rex() options
  tar_target(rex, tidy_rex(data_raw, years = reporting_years, desnz = progs_desnz)),
  tar_target(rex_last, tidy_rex_last(data_raw_last, years = reporting_years_last, desnz = progs_desnz)),


# 3. Tests ----
# various tests to check data entry errors
  tar_target(tests, data_checks(rp = reporting_periods, summary_main, rex)),


# 4. Transform ----
# create various summary tables

  # main summary table. kpi15 pulls the max reported figure rather than latest, ta kpi 1 is programme total (not portfolio total).
  tar_target(summary_main, transform_summary(rex, years = reporting_years)),
  # summary main using data cut from previous year
  tar_target(summary_main_last, transform_summary(rex_last, years = reporting_years_last)),
  # kpi15 summary 
  tar_target(summary_kpi15, transform_kpi15(summary_main, years = reporting_years, periods = reporting_periods, scores = kpi15_scores)),
  # ta01 summary by years - for future time series
  tar_target(summary_ta01, transform_ta01(rex, period = reporting_periods)),
  # kpi totals by year achieved, planned and tpb
  tar_target(summary_kpi_totals, transform_kpi_totals(summary_main, years = reporting_years, periods = reporting_periods, ta01 = summary_ta01)),
  # progs reporting each kpi
  tar_target(summary_kpi_progs,
             summary_main %>%
               filter(achieved_total > 0) %>%
               group_by(kpi_id, kpi_title) %>%
               distinct(programme_id)  %>%
               count() %>% 
               ungroup()
             ),
  # achieved and tpb by programme and kpi
  tar_target(summary_prog_achieved_total,
               summary_main %>%
                 group_by(programme_title, programme_id, kpi_id, pick(starts_with("disagg_"))) %>%
                 select(achieved_total, adj_tpb) %>%
                 summarise_if(is.numeric, sum, na.rm=T)
             ),
  # kpis reported on by each prog and last year they reported for that kpi
  tar_target(summary_prog_kpi_count_year, transform_prog_kpi_count(summary_main)),
  # kpis reported on by each prog
  tar_target(summary_prog_kpi_count_total, transform_prog_kpi_count(summary_main, include_last_year=F)),
  # top reporting programmes for each kpi for the current reporting year
  tar_target(summary_top_progs, transform_top_progs(summary_main)),
  # cumulative totals for each kpi, grouped by kpi
  tar_group_by(summary_cumsum, transform_cumsum(summary_main, years = reporting_years), kpi_id),
  # summary with differences to last year.
  tar_target(summary_results_last, transform_difference_to_last(data_results_last, summary_kpi_totals, periods = reporting_periods)),

  ## disaggegations
  # grouped data filtering kpis for which we don't yet report disaggregations
  tar_group_by(summary_main_grouped, summary_main %>%
                                        filter(!str_detect(kpi_id, "08|10|15|ICF KPI 02.2")), kpi_id),
  # fcdo and defra disaggs
  tar_target(summary_disagg_rex, disagg(summary_main_grouped),
                             pattern = map(summary_main_grouped),
                             iteration = "vector",
                              ),
  # grouped data filtering kpis for which we don't report disaggregations
  tar_group_by(desnz_disagg_grouped, data_raw_desnz_disagg %>%
                                        filter(!str_detect(kpi_id, "08|10|15|ICF KPI 02.2")), kpi_id),
  # desnz disaggs
  tar_target(summary_disagg_desnz, disagg_desnz(x = desnz_disagg_grouped, y = summary_main_grouped),
                            pattern = map(desnz_disagg_grouped),
                            iteration = "vector",
                            ),
  # main disagg table with disagg and disagg categories
  tar_target(summary_disagg_main, disagg_join(summary_disagg_rex, summary_disagg_desnz, summary_kpi_totals)),
  # no of progs reporting each disagg
  tar_target(summary_disagg_prog_count, disagg_prog_count(summary_main, data_raw_desnz_disagg, summary_kpi_progs)),

# 5. Tables ----

  # make tables closer to publication format
  tar_target(ta01_table, table_ta01(summary_ta01)),
  tar_target(kpi_totals_table, table_kpi_totals(summary_kpi_totals, summary_disagg_main, summary_kpi15, TRUE)),
  tar_target(kpi12_oecd_return_table, table_kpi12_oecd_return(summary_main)),
  tar_target(disagg_table, table_disagg(summary_disagg_main)),
  tar_target(data_table, table_cumsum(summary_cumsum, reporting_periods)),
  tar_target(prog_list_table, table_prog_list(summary_main)),
  tar_target(tpb_table, table_tpb(summary_main)),

# 6. Plots ----

  tar_target(line_plot, plot_cumsum(summary_cumsum, limit_factor=1.18), pattern = map(summary_cumsum), iteration = "list"),
  tar_target(kpi15_plot_new, plot_kpi15(summary_kpi15, scale = "new", colors = af_categorical_colours)),
  tar_target(kpi15_plot_old, plot_kpi15(summary_kpi15, scale = "old", colors = af_categorical_colours)),
  tar_target(ta01_plot, plot_kpita01(summary_disagg_main)),


# 7. Write ----

  tar_target(write_data_table, write_csv(data_table, file = here("tables","data-table.csv"))),
  #line_plots indexing which we want to output 
  tar_target(write_cumsum_plots, map(line_plot[1:11], 
                                     ~write_plots(.x, name=str_to_lower(.x$labels$title) %>% 
                                                    str_remove_all("\n") %>% 
                                                    str_replace_all(" ","_")))),
  tar_target(write_kpi15_plot_new, write_plots(kpi15_plot_new, name="kpi15_new")),
  tar_target(write_kpi15_plot_old, write_plots(kpi15_plot_old, name="kpi15_old")),
  tar_target(write_ta01_plot, write_plots(ta01_plot, name="ta01")),

# 8. Render ----

  #tar_render(report_word, "doc/icf_report.Rmd", output_dir = "report/", output_format=c("word_document")),
  #tar_render(report_html, "doc/icf_report.Rmd", output_dir = "report/", output_format=c("html_document")),
  #tar_render(report_md, "doc/icf_report.Rmd", output_dir = "report/", output_format=c("md_document")),
  #tar_render(report_pdf, "doc/icf_report_pdf.Rmd", output_dir = "report/", output_format=c("pdf_document")),

)



