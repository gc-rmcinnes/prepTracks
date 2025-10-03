#' @title extract from from yearly rasters
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
#' 
extract_by_year <- function(DT, var, startyr, endyr, maxyr, where = 'end', out = NULL){
  #lyr <- vect(layer)
  object_name <- deparse(substitute(var))
  yrs <- startyr:endyr
  ls_rast <-c(paste0(var, '_', yrs, '.tif'))
  names(ls_rast) <- as.character(yrs)
  # one year with gps data but no yearly rasters
  if(endyr < maxyr){
    newyrs <- (endyr+1):maxyr
    for (yy in newyrs) {
      ls_rast[[as.character(yy)]] <- c(paste0(var, '_', endyr, '.tif'))
    }
    
  }
  
  
  coords_start  <-  c('x1_', 'y1_')
  coords_end  <-  c('x2_', 'y2_')
  
  
  if (where == 'end') {
    DT[, paste(object_name, 'end', sep = "_") := 
         terra::extract(rast(ls_rast[as.integer(names(ls_rast)) == .BY[[1]]]),
      .SD,
      cells = FALSE,
      xy = FALSE,
      ID = FALSE
    ),
    by = year,
    .SDcols = c(coords_end)]
    
    if (is.null(out)){
      return(DT)
    }
    
    if (out == 'new'){
      return(DT %>% 
               dplyr::select(paste(object_name, 'start', sep = "_"))
      )
    }
  }
  
  if(where == 'start'){
    DT[, paste(object_name, 'start', sep = "_") := terra::extract(
      rast(ls_rast[as.integer(names(ls_rast)) == .BY[[1]]]),
      .SD,
      cells = FALSE,
      xy = FALSE,
      ID = FALSE
    ),
    by = year,
    .SDcols = c(coords_start)]
    
    if (is.null(out)){
      return(DT)
    }
    
    if (out == 'new'){
      return(DT %>% 
               dplyr::select(paste(object_name, 'end', sep = "_"))
      )
    }
  }
  
  if(where == 'both'){
    DT[, paste(object_name, 'start', sep = "_") := terra::extract(
      rast(ls_rast[as.integer(names(ls_rast)) == .BY[[1]]]),
      .SD,
      cells = FALSE,
      xy = FALSE,
      ID = FALSE
    ),
    by = year,
    .SDcols = c(coords_start)]
    
    DT[, paste(object_name, 'end', sep = "_") := terra::extract(
      rast(ls_rast[as.integer(names(ls_rast)) == .BY[[1]]]),
      .SD,
      cells = FALSE,
      xy = FALSE,
      ID = FALSE
    ),
    by = year,
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