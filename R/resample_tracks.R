#' @title check resample tracks
#' @export
#' @author Julie W. Turner
#'
resample_tracks <- function(tracks, rate, tolerance, probsfilter, longlat) {
  t <- track_resample(tracks, rate = rate, tolerance = tolerance) %>%
    filter_min_n_burst()

  # remove bursts with less than 4 steps
  t <- t %>%
    dplyr::group_by(burst_) %>%
    dplyr::filter(dplyr::n() >= 4) %>%
    dplyr::ungroup()

  # Cancel if there are not at least 20 observed steps after resample
  # this is semi-arbitrary, but this should be enough for robust estimates in the model
  # (Street et al preprint 2021)
  if (nrow(t) < 20) return()
  # filter out steps that are outside the probsfilter%: there were unnaturally long steps
  t %>% steps_by_burst(., lonlat = longlat, keep_cols = 'start') %>%
    dplyr::filter(sl_<=quantile(sl_, probs = probsfilter, na.rm = T))
}

