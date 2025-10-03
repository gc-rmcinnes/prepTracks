#' @title Merge LC with description
#' @export
#' @author Julie W. Turner
#' 
make_mergelc <- function(DT, meta, byname) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
   merge(
    DT,
    meta[,.(value, lc_end = becomes)],
    by.x = byname,
    by.y = 'value',
    all.x = TRUE
  )
 
}
