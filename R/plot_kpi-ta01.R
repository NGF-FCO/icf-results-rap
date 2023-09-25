#' plot countries supported with TA
#' 
#' @param disagg  summary_disagg_main
#' 


plot_kpita01 <- function(disagg, 
                         color1 = "grey80",
                         color2 = "grey60",
                         fill1 = "grey80",
                         fill2 = "grey50",
                         bgcol = "#ffffff"){
  
countries <- disagg  %>%
                ungroup() %>% 
                filter(kpi_id=="ICF TA KPI 01" & disagg=="disagg_country" & !category=="unspecified") %>%
                mutate(category = str_to_title(category)) %>% 
                mutate(category = str_replace(category, " And ", " and ")) %>% 
                mutate(category = case_when(str_detect(category, "Myan") ~ "Myanmar", TRUE ~ category)) %>% 
                select(category) %>% 
                pull(category) 

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  mutate(reached =
           case_when(
             name %in% countries  ~ "yes",
             name == "Antigua and Barb." ~ "yes",	
             name == "Bosnia and Herz." ~ "yes",	
             name == "Cape Verde" ~ "yes",	
             name == "Cayman Is." ~ "yes",
             name == "Central African Rep." ~ "yes",	
             name == "Côte d'Ivoire" ~ "yes",	
             name == "Curaçao" ~ "yes",	
             name == "Dem. Rep. Congo" ~ "yes",	
             name == "Dominican Rep." ~ "yes",	
             name == "Eq. Guinea" ~ "yes",
             name == "Swaziland" ~ "yes",
             name == "Lao PDR	Laos" ~ "yes",
             name == "Marshall Is." ~ "yes",	
             name == "Macedonia" ~ "yes",
             name == "Palestine" ~ "yes",	
             name == "Congo" ~ "yes",
             name == "St. Kitts and Nevis" ~ "yes",	
             name == "St. Vin. and Gren." ~ "yes",	
             str_detect(name, "Principe") ~ "yes",	
             name == "Sint Maarten" ~ "yes",	
             name == "Bahamas" ~ "yes",	
             name == "Gambia" ~ "yes",	
             name == "Trinidad and Tobago" ~ "yes",	
             name == "Turks and Caicos Is." ~ "yes",	
             name == "Lao PDR" ~ "yes",
             TRUE ~ "no"
           ))


y <- ggplot(world) +
        geom_sf(aes(fill=reached, color=reached),lwd=0.01) +
        coord_sf(ylim = c(-60, 80), expand = T) +
        theme_bw()+
        ditch_the_axes +
        labs(fill=NULL) +
        scale_fill_manual(values=c(fill1, fill2), labels = c("Not Supported", "Supported")) +
        scale_color_manual(values=c(color1,color2), guide="none") +
        theme(
          legend.position = "none",
          panel.background = element_rect(fill = bgcol),
          aspect.ratio=5/10
        )

return(y)

}