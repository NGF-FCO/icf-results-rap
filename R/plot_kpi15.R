#' plots for new and old kpi15 scales
#' 
#' @param x  summary_kpi15 target
#' @param scale  "new" or "old" - will plot differently
#' @param title_font  "gds_bold" see _targets front matter
#' @param subtitle_font "gds" see _targets front matter
#' @param axis_font  "roboto" see _targets front matter
#' @param subtitle_width  50 
#' @param colors  af_categorical_colours from accessory_funs
#' @param y_scale  1e-6 for tweaking
#' @param limit_factor 1.2 for tweaking

plot_kpi15 <- function(x, 
                       scale = "new",
                       title_font = "gds_bold", 
                       subtitle_font = "gds", 
                       axis_font = "roboto", 
                       subtitle_width = 50, 
                       colors = af_categorical_colours,
                       y_scale = 1e-6,
                       limit_factor = 1.2,
                       title_size = 14,
                       subtitle_size = 12,
                       axis_text_size = 10){
  
  if(!scale %in% c("new", "old")){
    stop("Invalid value provided for scale.\nMust be one of 'old' or 'new'")
  }

  if(scale=="new"){
    
      totals <- x %>% 
                    filter(scale == "new") %>% 
                    group_by(achieved) %>% 
                    count()
       
       y <-  ggplot(totals, aes(x = achieved, y = n)) +
                geom_col(fill="#12436D") +
                geom_text(aes(label = n, y = n, fontface=2),
                          vjust = ifelse(totals$n <= 6, -0.2, 1.5), 
                          color = ifelse(totals$n<=6,"black","white"),
                          size = 4 ) +
                icf_style(title_font = title_font, 
                          axis_font = axis_font, 
                          subtitle_font = subtitle_font,
                          title_size = 14,
                          subtitle_size = 12,
                          axis_text_size = 10) +
                labs(title = paste0("ICF KPI 15", "\n"),
                     subtitle = "Number of Programmes",
                     x = "Score")+
                theme(axis.title.x = element_text(size=12),
                      axis.title.y = element_blank(),
                      ) +
                scale_y_continuous(breaks = pretty(totals$n), 
                                   limits = c(0, max(totals$n) * limit_factor)
                                   )
       
       return(y)
      
  }else{
    
      annual_total <- x %>%
                          filter(scale=="old") %>% 
                          group_by(period) %>% 
                          count(name = "total") %>% 
                          mutate(total = case_when(
                                                  total < 10 ~ "(<10)",
                                                  TRUE ~ paste0("(",as.character(total),")")
                                                  )
                                ) %>% 
                          mutate(period = basename(period))
    
      
      y <- x %>% 
              filter(scale=="old") %>% 
              group_by(period, achieved) %>% 
              count() %>% 
              mutate(score_description = case_when(
                achieved == 0 ~ "0: Tranformational changed unlikely",
                achieved == 1 ~ "1: No evidence of change available",
                achieved == 2 ~ "2: Early evidence suggests change likely",
                achieved == 3 ~ "3: Tentative evidence evidence change likely",
                achieved == 4 ~ "4: Clear evidence evidence change very likely",
              )) %>% 
              mutate(period = basename(period)) %>% 
                  ggplot(aes(x = period, y = n, fill = score_description, group = achieved)) +
                  geom_bar(position = "fill", stat = "identity", color = "black") +
                  geom_text(data = annual_total, aes(x = period, y = 0.99, label = total, fill = NULL, group = NULL),  size=4,vjust=-0.75) +
                  labs(x="", y="Proportion\n", col="", title="\n" ) +
                  scale_fill_manual(values = af_categorical_colours) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), expand = c(0,0)) +
                  icf_style(title_size = 14,
                            subtitle_size = 12,
                            axis_text_size = 10) +
                  labs(title = "ICF KPI 15",
                       subtitle = "Proportion of Scores by Year (and Number of Programmes)\n") +
                  theme(axis.title.y = element_blank(),
                        legend.position = "bottom",
                        legend.title = element_blank()) +
                  guides(fill = guide_legend(nrow = 3, byrow = FALSE)) +
                  coord_cartesian(ylim = c(0, 1.1), clip = "off")
      
       return(y)
      
  }


}
  

  

