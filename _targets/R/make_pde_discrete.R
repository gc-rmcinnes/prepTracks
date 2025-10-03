#' @title make pde levels discrete
#' @export
#' @author Julie W. Turner
#' 
make_pde_discrete <- function(file.path, breaks, season){
  rr <- rast(file.path)
  breaks <- as.double(breaks)
  pde.discrete <- classify(rr, breaks, include.lowest=TRUE, brackets=TRUE)
  path <- file.path('data', 'derived', paste0('pde_', season, '_disc10.tif'))
  writeRaster(pde.discrete, path, overwrite = T)
  path
}