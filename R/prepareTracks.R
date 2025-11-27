prepareTracks <- function(crs,
                          interval,
                          sl.interval,
                          minyr,
                          maxyr,
                          rate,
                          tolerance,
                          locations,
                          probsfilter){
  
  id <- "id"
  datetime <- "datetime"
  longlat <- FALSE
  # not actually longitude and latitude, just don\'t want to change code
  long <- "x"
  lat <- "y"
  
  # minimum year we want to pull data for
  
  # Split by: within which column or set of columns (eg. c(id, yr))
  # do we want to split our analysis?
  splitBy <- id
  
  input <- as.data.table(locations)
  mkunique <- make_unique_complete(input, id, datetime, long, lat)
  subdt <- mkunique[lubridate::year(datetime) >= minyr]
  splits <- split(subdt, by = splitBy, keep.by = FALSE)
  splitsnames <- unique(subdt[, .(n_points = .N), by = splitBy])
  tracks <- lapply(splits, function(s) {
    make_track(s, long, lat, datetime, crs = crs, all_cols = TRUE)
  })
  resamples <- lapply(tracks, function(t) {
    resample_tracks(t, rate, tolerance, probsfilter = probsfilter, longlat)
  })
  resamples_combined <- data.table::rbindlist(resamples, idcol = "original_list_id")
  distributions <- ggplot(resamples_combined, aes(sl_)) + geom_density(alpha = 0.4)
  sl_distr <- fit_distr(resamples_combined$sl_, "gamma")
  ta_distr <- fit_distr(resamples_combined$ta_, "vonmises")
  # Note: This maps over the original 'resamples' list, not the combined one.
  randsteps <- lapply(resamples, function(r) {
    make_random_steps(r, sl_distr, ta_distr)
  })
  distparams <- lapply(X = randsteps, FUN = calc_distribution_parameters)
  # This function likely takes the list and combines it, e.g., using rbindlist().
  dattab <- make_data_table2(randsteps)
  # Use copy() to avoid modifying 'dattab' in place
  addyear <- data.table::copy(dattab)
  addyear[, `:=`(
    year = lubridate::year(t2_),
    int.year = plyr::round_any(lubridate::year(t2_), interval, floor)
  )]
  stepID <- data.table::copy(addyear)
  stepID[, indiv_step_id := paste(id, step_id_, sep = "_")]
 buffer <- plyr::round_any(median(addyear$sl_, na.rm = TRUE), sl.interval, floor)
 
 return(list(tracks = stepID,
             distparams = distparams,
             buffer = buffer))
}