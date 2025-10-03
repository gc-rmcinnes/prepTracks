#' @title Load a sf object
#' @export
#' @author Julie W. Turner
#' 
load_sf<- function(obj, outcrs) {
  sf <- st_read(obj)
  if(st_crs(sf)$wkt == st_crs(outcrs)$wkt)
    return(sf)
  else
    st_transform(sf, outcrs)
}