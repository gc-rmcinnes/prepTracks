#' @title extract from fire polygons
#' @export
#' @author Julie W. Turner
#' 
extract_fire <- function(DT, layer, step.end  = T){
  lyr <- vect(layer)
  object_name <- deparse(substitute(layer))
  
  lyr.sub
  
  if (isTRUE(step.end)) {
    coords <-  c('x2_', 'y2_')
    object_end <- 'end'
  }
  
  if(isFALSE(step.end)){
    coords <-  c('x1_', 'y1_') 
    object_end <- 'start'
  }
  
  DT[,(paste(object_name, object_end, sep = "_")):= 
       terra::extract(subset(lyr, lyr$YEAR<= year(datetime)), cbind(.SD), fun = max)[,-1],
     .SDcols = c(coords)]
  
}