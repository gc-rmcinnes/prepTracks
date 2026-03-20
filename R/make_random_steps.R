#' @title make random steps
#' @export
#' @author Julie W. Turner
#'
make_random_steps <- function(DT, sl, ta) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()

  #DT <- DT[!is.na(ta_) & sl_ > 0]

  random_steps(DT, n_control = 10, sl_distr = sl, ta_distr = ta)
  # %>%
  #   time_of_day(where = 'start', include.crepuscule = F)
}
