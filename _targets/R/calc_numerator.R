#' @title calc numerator for PDE maps
#' @export
#' @author Julie W. Turner
#' 
calc_numerator <- function(rasts, season){
  r1 <- rast(rasts)
  # r2 <- rast(rast2)
  # r3 <- rast(rast3)
  # r2 <- crop(extend(rast(rast2), r1), r1)
  # r3 <- crop(extend(rast(rast3), r1), r1)
  # r1 <- extend(r1, r3)
  # r2 <- extend(r2, r3)
  # r3 <- crop(r3, r1)
 
  numerator <- exp(sum(r1))
  numerator[is.infinite(numerator)] <- NA
  path <- file.path('data', 'derived', 'temp', paste(season, 'numerator.tif', sep='_'))
  writeRaster(numerator, path, overwrite = T)
  path
}