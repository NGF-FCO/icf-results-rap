#' calculates what periods (YYYY/YYYY) are in scope for current publication
#' 

get_reporting_periods <- function(years){
    
  x <- map_chr(years,function(x) as.character(paste0(x, "/", as.numeric(x) + 1)))
  
  return(x)
  
}





       
