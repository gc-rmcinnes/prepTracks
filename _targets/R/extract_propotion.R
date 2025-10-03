#' @title extract proportion within buffer
#' @export
#' @author Julie W. Turner
#' 
extract_proportion <- function(DT, feature, landclass, buff, crs, where = 'end') {
  # setting this up for splitby mapping in targets
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  yrs <- c(2010,2015,2020)
  ls_rast <-c(paste0(feature, '_', yrs, '.tif'))
  names(ls_rast) <- as.character(yrs)
  yr <- unique(DT$int.year)
  
  # select the right raster
  feat <- rast(ls_rast[as.character(yr)])
  object_name <- paste(deparse(substitute(feature)))
  
  coords_start  <-  c('x1_', 'y1_')
  coords_end  <-  c('x2_', 'y2_')
  
  DT[,pt:= 1:.N, by = .(id, step_id_)]
  DT[,step_pt_id:= paste(id, step_id_, pt, sep = '.')]
  
  
  if(where == 'start'){
    samp <- DT[,sample_lsm(feat, st_as_sf(.SD, coords = coords_start, crs = crs), 
                           what = "lsm_c_pland", size = buff, shape = 'circle', 
                           plot_id = paste(id, step_id_, pt, sep = '.'))]
    classd <- setDT(merge(samp, landclass, by.x = 'class', by.y = 'value'))
    classd[,`:=` (landtype = paste(becomes, 'start', sep = '_'), value = value/100)]
    
    
    transDT <- dcast(classd[,.(plot_id, landtype, value)], plot_id ~ landtype, 
                     value.var = 'value', fun.aggregate = sum, fill = 0)
    mrg <- merge(DT, transDT, by.x = 'step_pt_id', by.y = 'plot_id')
  }
  
  if(where == 'end'){
    samp <- DT[,sample_lsm(feat, st_as_sf(.SD, coords = coords_end, crs = crs), 
                           what = "lsm_c_pland", size = buff, shape = 'circle', 
                           plot_id = paste(id, step_id_, pt, sep = '.'))]
    classd <- setDT(merge(samp, landclass, by.x = 'class', by.y = 'value'))
    classd[,`:=` (landtype = paste(becomes, 'end', sep = '_'), value = value/100)]
    
    
    transDT <- dcast(classd[,.(plot_id, landtype, value)], plot_id ~ landtype, 
                     value.var = 'value', fun.aggregate = sum, fill = 0)
    mrg <- merge(DT, transDT, by.x = 'step_pt_id', by.y = 'plot_id')
  }
  
  if(where == 'both'){
    samp.start <- DT[,sample_lsm(feat, st_as_sf(.SD, coords = coords_start, crs = crs), 
                                 what = "lsm_c_pland", size = buff, shape = 'circle', 
                                 plot_id = paste(id, step_id_, pt, sep = '.'))]
    classd.start <- setDT(merge(samp.start, landclass, by.x = 'class', by.y = 'value'))
    classd.start[,`:=` (landtype = paste(becomes, 'start', sep = '_'), value = value/100)]
    
    
    transDT.start <- dcast(classd.start[,.(plot_id, landtype, value)], plot_id ~ landtype, 
                           value.var = 'value', fun.aggregate = sum, fill = 0)
    
    mrg.start <- merge(DT, transDT.start, by.x = 'step_pt_id', by.y = 'plot_id')
    
    samp.end <- DT[,sample_lsm(feat, st_as_sf(.SD, coords = coords_end, crs = crs), 
                               what = "lsm_c_pland", size = buff, shape = 'circle', 
                               plot_id = paste(id, step_id_, pt, sep = '.'))]
    classd.end <- setDT(merge(samp.end, landclass, by.x = 'class', by.y = 'value'))
    classd.end[,`:=` (landtype = paste(becomes, 'end', sep = '_'), value = value/100)]
    
    
    transDT.end <- dcast(classd.end[,.(plot_id, landtype, value)], plot_id ~ landtype, 
                         value.var = 'value', fun.aggregate = sum, fill = 0)
    
    mrg <- merge(mrg.start, transDT.end, by.x = 'step_pt_id', by.y = 'plot_id')
  }
  
  return(mrg)
}

