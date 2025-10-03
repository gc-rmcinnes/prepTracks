#' @title extract data points
#' @export
#' @author Julie W. Turner
#' 
extract_pt <- function(DT, layer, name, where, out = NULL, yr = NULL, int.yr= NULL){
  lyr <- rast(layer)
  object_name <- name
  #object_name <- deparse(substitute(name))
  
  coords_start  <-  c('x1_', 'y1_')
  coords_end  <-  c('x2_', 'y2_')
  
  if(!is.null(yr)){
    DT <- DT[year == yr]
    DT
  }
  if(!is.null(int.yr)){
    DT <- DT[int.year == int.yr]
    DT
  }
  
  if (where == 'end') {
    DT[,(paste(object_name, 'end', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_end)]
    if (is.null(out)){
      return(DT)
    }
    if (out == 'new'){
      return(DT %>% dplyr::select(last_col()))
    }
  }
  
  if (where == 'start') {
    DT[,(paste(object_name, 'start', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_start)]
    if (is.null(out)){
      return(DT)
    }
    if (out == 'new'){
      return(DT %>% dplyr::select(last_col()))
    }
  }
  
  if (where == 'both') {
    DT[,(paste(object_name, 'start', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_start)]
    DT[,(paste(object_name, 'end', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_end)]
    
    if (is.null(out)){
      return(DT)
    }
    
    if (out == 'new'){
      return(DT %>% 
               dplyr::select(paste(object_name, 'start', sep = "_"), paste(object_name, 'end', sep = "_"))
             )
    }
  }
  
}