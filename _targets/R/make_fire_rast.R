#' @title make rasters of most recent fire by year
#' @export
#' @author Julie W. Turner
#' 
make_fire_rast <- function(layer, rast){
  lyr <- vect(layer)
  raster <- rast(rast)
  yearsWithData <- unique(lyr[["YEAR"]])$YEAR
  
  historicFireRasters <- lapply(yearsWithData, function(yr) {
    subst <- subset(lyr, lyr$YEAR<= yr)
    #fires <- 
      suppressWarnings({
        rasterize(subst, raster, field="YEAR", fun=max)
      })
    
  })
  names(historicFireRasters) <- yearsWithData
  historicFireStack <- rast(historicFireRasters)
  #saveRDS(historicFireRasters, file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires_list.RDS'))
  fnames <- paste0("fires_", yearsWithData, ".tif")
  writeRaster(historicFireStack, fnames)
}