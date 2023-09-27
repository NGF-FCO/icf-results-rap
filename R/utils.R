#' utility functions used throughout the pipeline



#' Basically just get rid of any and all lines
#' @export
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


#' rounding up or down
#' @param x numeric vector
#' @param roundTo integer
#' @param dir logical describing direction up or down
#' @keywords internal
#' @export
round_choose <- function(x, roundTo, up=FALSE) {
  if(up == TRUE) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(up == FALSE) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}


#' theming for icf plots
icf_style <- function(title_font = "gds_bold", 
                      subtitle_font = "gds", 
                      axis_font = "roboto",
                      title_size = 14,
                      subtitle_size = 12,
                      axis_text_size = 10
                      ){
  ggplot2::theme(plot.title = ggplot2::element_text(family = title_font, 
                                                    size = title_size, face = "bold", color = "#222222"), 
                 plot.title.position = "plot",
                 plot.subtitle = ggplot2::element_text(family = subtitle_font, 
                                                    size = subtitle_size),
                
                 plot.caption = ggplot2::element_blank(), 
                 legend.position = "top", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = axis_font, size = 10, 
                                                     color = "#222222"), 
                 axis.title.y = ggplot2::element_text(family = axis_font, 
                                                      size = 14, 
                                                      color = "#222222", 
                                                      angle = 0), 
                 axis.text.y = ggplot2::element_text(family = axis_font, size = axis_text_size, color = "#222222"), 
                 axis.text.x = ggplot2::element_text(family = axis_font, size = axis_text_size, color = "#222222", margin = ggplot2::margin(5, b = 8)), 
                 axis.ticks = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"), 
                 panel.grid.major.x = ggplot2::element_blank(), 
                 panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(size = 22, hjust = 0)
                 )
}
  
#' ukgov analysis function categorical colors
af_categorical_colours <- c(
    "#12436D", # Dark blue
    "#28A197", # Turquoise
    "#801650", # Dark pink
    "#F46A25", # Orange
    "#3D3D3D", # Dark grey
    "#A285D1" # Light purple
  )

#' can bind vectors of unequal length
cbind.fill <- function(...){
    nm <- list(...) 
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
      rbind(x, matrix(, n-nrow(x), ncol(x))))) 
  }

# wrapper for filter/pull chains  
pull_data <- function(x, filter_val, filter_col, pull_col){
                  x %>% 
                  filter(!!sym(filter_col)==filter_val) %>% 
                  pull(!!sym(pull_col))
                }
  
  
