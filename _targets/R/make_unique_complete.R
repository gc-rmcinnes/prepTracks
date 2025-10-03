#' @title Make unique and complete
#' @export
#' @author Julie W. Turner
#' 
make_unique_complete <- function(DT, id, datetime, long, lat) {
  no.na <- na.omit(unique(DT, by = c(id, datetime)),
          cols = c(long, lat, datetime))
  setDT(no.na)[,npts := .N, by = .(id)]
  dat3 <- no.na[npts>=3]
  dat3
}
