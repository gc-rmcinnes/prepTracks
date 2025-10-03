#' @title calc constant for PDE maps
#' @export
#' @author Julie W. Turner
#' 
calc_constant <- function(numerator){
  global(rast(numerator), sum, na.rm = T)
}