#' @title calculate distribution parameters
#' @export
#' @author Julie W. Turner
#' 
calc_distribution_parameters <- function(steps) {
  if (is.null(steps)) return()
  c(ta_distr_params(steps), sl_distr_params(steps))
}
