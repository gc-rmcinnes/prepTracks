#' @title setup to reclassify with beta values for numerator
#' @export
#' @author Julie W. Turner
#' 
make_rastcoef <- function(file.path, var, coefs, season, extent){

  rr <- resample(rast(file.path), rast(extent))
  termcoef = as.double(coefs[term %like% var, .(estimate)])
  ## reclassifly land with betas
  rastcoef <- (2*termcoef)*rr
  path <- file.path('data', 'derived', 'temp', paste(var, season, 'coef.tif', sep='_'))
  writeRaster(rastcoef, path, overwrite = T)
  path
 
}