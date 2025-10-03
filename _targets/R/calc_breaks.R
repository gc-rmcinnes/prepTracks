#' @title calculate quantile breaks
#' @export
#' @author Julie W. Turner
#' 
calc_breaks <- function(file.path){
  breaks <- global(rast(file.path), quantile, na.rm = T, probs = seq(0,1,.1))
  v.breaks <- unname(as.vector(breaks))
  as.vector(t(v.breaks))
}