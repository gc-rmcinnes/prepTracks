#' @title calc PDE maps
#' @export
#' @author Julie W. Turner
#' 
calc_pde <- function(numerator, constant, season){
  num <- rast(numerator)
  
  div <- function(x){
    C <- as.double(constant)
    pde <- x/C
    return(pde)
  }
  path <- file.path('data', 'derived', 'temp', paste0('pde_', season, '.tif'))
  pde <- app(num, div, filename = path, overwrite = T)

  return(path)
}