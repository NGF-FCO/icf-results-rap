#' line plots for cumulative kpis
#' @param df summary_cumsum target
#' @param title_font short name from font_add using fonts in the font/ directory
#' @param subtitle_font short name from font_add using fonts in the font/ directory
#' @param axis_font short name from font_add using fonts in the font/ directory
#' @param subtitle_width width for wrapping long subtitles
#' @param y_units units to use for y axis passed to labels unit_format
#' @param y_scale increments to use for y axis passed to labels unit_format, e.g. 1e-6
#' @param limit_factor value by which to multiply highest value to ensure points not out of range of scale
#'

plot_cumsum <- function(df, 
                        title_font = "gds_bold", 
                        subtitle_font = "gds", 
                        axis_font = "roboto", 
                        subtitle_width = 50, 
                        y_scale = 1e-6,
                        limit_factor = 1.2){

  kpi_id <- unique(df$kpi_id)
  kpi_plot_title <- kpi_id %>% str_replace("I 0", "I ")
  highest_value <- max(df$achieved_cumulative)
  unit <- case_when(
                str_detect(kpi_id, "06|10|11|12") ~ str_remove(unique(df$units), "[(].*") %>% str_trim(),
                TRUE ~ unique(df$units)
                )
  
  y_units_abbrev <- case_when(
    highest_value>=1e+09 ~ "B",
    highest_value < 1e+09 & highest_value>=1e+06 ~"M",
    highest_value < 1e+06 & highest_value > 1000 ~ "K",
    TRUE ~ "")
  
  y_units <- case_when(
    highest_value>=1e+09 ~ "Billions",
    highest_value < 1e+09 & highest_value>=1e+06 ~"Millions",
    highest_value < 1e+06 & highest_value > 1000 ~ "Thousands",
    TRUE ~ "")
  
  y_scale <- case_when(
    highest_value>=1e+09 ~ 1e-09,
    highest_value < 1e+09 & highest_value>=1e+06 ~ 1e-06,
    highest_value < 1e+06 & highest_value > 1000 ~ 1e-03,
    TRUE ~ 1)
  
 ggplot(df, aes(x=year, y = achieved_cumulative, group=1)) +
    geom_hline(yintercept = 0, linewidth = 1, colour="#333333") +
    geom_line(linewidth = 2, alpha= 0.8, color = "#12436D") +
    geom_point(size = 5, alpha = 1, color = "#12436D") +
    icf_style(title_font = title_font, 
              axis_font = axis_font, 
              subtitle_font = subtitle_font,
              title_size = 14,
              subtitle_size = 12,
              axis_text_size = 10) +
    labs(title= paste0(kpi_plot_title, "\n"),
         subtitle = case_when(
           nchar(y_units) > 1 ~ paste0(unit, " (", y_units,")"),
           TRUE ~ unit),
         y ="" ,
         x =""
        ) +
    scale_y_continuous(breaks = pretty(df$achieved_cumulative), 
                       limits = c(0, highest_value * limit_factor), 
                       labels = unit_format(unit = y_units_abbrev, scale = y_scale))

  }


