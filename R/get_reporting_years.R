#' calculates what years are in scope for current publication
#' 

get_reporting_years <- function(){
    
  x <- as.character(2011:as.numeric(format(Sys.Date() %m-% years(1), "%Y")))
  
  return(x)
  
}





       
