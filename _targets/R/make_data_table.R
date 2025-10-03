#' @title make data table
#' @export
#' @author Julie W. Turner
#' 
make_data_table <- function(DT) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  as.data.table(DT)
}