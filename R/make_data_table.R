#' @title make data table
#' @export
#' @author Julie W. Turner
#' 
make_data_table <- function(DT) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  as.data.table(DT)
}

# Using this function as the other was not working properly (erroring)
make_data_table2 <- function(DT) {
  fDT <- Filter(function(x) !is.null(x) && nrow(x) > 0, DT)
  ffDT <- rbindlist(lapply(names(fDT), function(eachID){
    intDT <- data.table(fDT[[eachID]])
    intDT[, id := eachID]
    return(intDT)
  }))
  return(ffDT)
}
