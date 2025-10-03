#' @title make random steps
#' @export
#' @author Julie W. Turner
#' 
make_random_steps <- function(DT) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()

  random_steps(DT, n_control = 10) 
  # %>%
  #   time_of_day(where = 'start', include.crepuscule = F)
}