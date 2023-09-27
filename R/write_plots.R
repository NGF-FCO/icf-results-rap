#' write plots using ggsave.
#' 
#' @param x  any target
#' @param name name of file for printing
#' @param format format- default svg but can be png jpeg
#' @param resize taken as base dims for gov.uk x by resize
#' 

write_plots <- function(x, name, format="svg", resize=2){

ggsave(filename=paste0("images/", name,".", format),
       plot=x, 
       dpi=300, 
       width = 960*resize, 
       height = 640*resize, 
       units="px")
  
}



