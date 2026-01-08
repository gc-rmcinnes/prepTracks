prepareTracks <- function(crs,
                          interval,
                          sl.interval,
                          minyr,
                          maxyr,
                          rate,
                          tolerance,
                          locations,
                          probsfilter, 
                          aggrNonAnnualData,...){
  
  id <- "id"
  datetime <- "datetime"
  longlat <- FALSE
  # not actually longitude and latitude, just don't want to change code
  long <- "x"
  lat <- "y"

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
  randsteps <- lapply(names(resamples), function(nr) {
    r <- resamples[[nr]]
    i   <- match(nr, names(resamples))
    tot   <- length(resamples)
    pct <- round(i / tot * 100, 1)
    # Print every 20% of total iterations
    int20 <- max(1, floor(tot / 5))  # Divide into 5 parts (20% each)
    
    if (i == 1 || i == tot || i %% int20 == 0)
      print(paste0("Making random steps for ", nr, ". ", pct,"% completed...(", i,"/", tot, ")"))  
    if (any(is.null(r),
            nrow(r) == 0)) return(NULL)
    # Only remove zero-length steps
    track_clean <- r %>%
      filter(sl_ > 0) %>%
      # Keep only bursts with enough data
      group_by(burst_) %>%
      filter(n() >= 3) %>%
      ungroup()
    
    # Restore the proper amt classes
    class(track_clean) <- class(r)
    
    rsteps <- random_steps(track_clean, n_control = 10, sl_distr = sl_distr, ta_distr = ta_distr)
    return(rsteps)
  })
  names(randsteps) <- names(resamples)
  distparams <- lapply(X = randsteps, FUN = calc_distribution_parameters)
  # This function takes the list and combines it, e.g., using rbindlist().
  dattab <- make_data_table2(randsteps)
  # Use copy() to avoid modifying 'dattab' in place
  addyear <- data.table::copy(dattab) # I don't think there is the need here 
                                      # it consumes memory for bigger tables... 
  message(paste0("Aggredating non-annual data: ", aggrNonAnnualData))
  if (aggrNonAnnualData == "middle"){
    addyear[, `:=`(
      year = lubridate::year(t2_),
      int.year = plyr::round_any(lubridate::year(t2_), interval, round)
    )]
  } else 
    if (aggrNonAnnualData == "sequencePre") {
      addyear[, `:=`(
        year = lubridate::year(t2_),
        int.year = plyr::round_any(lubridate::year(t2_), interval, ceiling)
      )]
      } else 
      if (aggrNonAnnualData == "sequencePos") {
        addyear[, `:=`(
          year = lubridate::year(t2_),
          int.year = plyr::round_any(lubridate::year(t2_), interval, floor)
        )]
      } else {
        stop(paste0("The parameter ", aggrNonAnnualData, " is not implemented. Please use: ",
                    "middle, sequencePre or sequencePos"))
      }

stepID <- data.table::copy(addyear)
stepID[, indiv_step_id := paste(id, step_id_, sep = "_")]
buffer <- plyr::round_any(median(addyear$sl_, na.rm = TRUE), sl.interval, floor)

message("Tracks created!")
 
 return(list(tracks = stepID,
             distparams = distparams,
             buffer = buffer))
}