defineModule(sim, list(
  name = "prepTracks",
  description = "A targets pipeline to prepare tracks derived from location data",
  keywords = "",
  authors = c(person("Julie", "Tuner", email = "", role = c("aut", "cre")),
              person("Rory", "McInnes", email = "", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(prepTracks = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "prepTracks.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9003)", "ggplot2", "targets", "tarchetypes", "amt", "data.table",
                  "terra", "sf", "sp", "ggplot2", 'plyr', "distanceto", "glmmTMB", "dtplyr", "glue"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "caribouLoc", objectClass = "data.table",
                 desc = "Harmonized and cleaned caribou locations of all jurisdictions provided")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "tracks", objectClass = "data.table",
                  desc = "Prepared tracks with random steps"),
    createsOutput(objectName = "distparams", objectClass = "list",
                  desc = "A list of parameters from a fitted distribution"),
    createsOutput(objectName = "buffer", objectClass = "numeric",
                  desc = "Buffer for moving windows")

  )
))

doEvent.prepTracks = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # run the targets pipeline that generates the tracks
      sim <- scheduleEvent(sim, time(sim), "prepTracks", "preparingTracks")

    },
    preparingTracks = {
      if (is.null(sim$caribouLoc)){
        stop("The object caribouLoc is NULL. Please debug.")
      }
      tracksReady <- prepareTracks(sim)

    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

# Init <- function(sim) {
#   # prepped locations for pipeline
#   sim_locations <- sim$caribouLoc
#
#   # set the paths for the targets pipeline
#   module_name <- currentModule(sim)
#   module_root_path <- modulePath(sim, module = module_name)
#   store_path <- file.path(module_root_path, module_name, "_targets_store")
#   script_path <- file.path(store_path, "_targets.R")
#   # R folder with custom functions
#   module_r_path <- file.path(module_root_path, module_name, "R")
#
#   # Write the _targets.R script on the fly
#   script_text <- glue('
#   # --- Code to run BEFORE the pipeline ---
#
#     # 1. Load the targets package
#     library(targets)
#     library(tarchetypes)
#     library(amt)
#     library(data.table)
#     library(terra)
#     library(sf)
#     library(sp)
#     library(ggplot2)
#     library(glmmTMB)
#     library(distanceto)
#     library(dtplyr)
#     print("Checking sim_locations exists:")
#     print(exists("sim_locations"))
#
#
#     # Source the module functions explicitly
#     tar_source(files = "{module_r_path}")
#
#     # Set envir so targets can access sim_locations
#     tar_option_set(
#       packages = c("dplyr", "ggplot2", "tarchetypes", "amt", "data.table",
#                    "terra", "sf", "sp", "glmmTMB", "distanceto"),
#       format = "qs",
#       envir = environment()
#     )
#
#     set.seed(37)
#
#     id <- "id"
#     datetime <- "datetime"
#     longlat = FALSE
#     # not actually longitude and latitude, just don\'t want to change code
#     long <- "x"
#     lat <- "y"
#     crs <- st_crs(3978)$wkt
#
#     # minimum year we want to pull data for
#     minyr <- 2010
#     maxyr <- 2022
#
#     # Split by: within which column or set of columns (eg. c(id, yr))
#     # do we want to split our analysis?
#     splitBy <- id
#     interval <- 5 # to round by 5 year intervals
#     sl.interval <- 50 # to round by 50m intervals
#
#     # Resampling rate
#     rate <- hours(13)
#
#     # Tolerance
#     tolerance <- minutes(150)
#
#     # Targets: prep -----------------------------------------------------------------
#     targets_prep <- c(
#       # Read input data
#       tar_target(
#         input,
#         as.data.table(sim_locations)
#       ),
#
#       # Remove duplicated and incomplete observations
#       tar_target(
#         mkunique,
#         make_unique_complete(input, id, datetime, long, lat)
#       ),
#       # subsample data to that greater than minimum year
#       tar_target(
#         subdt,
#         mkunique[lubridate::year(datetime) >= minyr]
#       ),
#
#       # Set up split -- these are our iteration units
#       tar_target(
#         splits,
#         subdt[, tar_group := .GRP, by = splitBy],
#         iteration = "group"
#       ),
#
#       tar_target(
#         splitsnames,
#         unique(subdt[, .(n_points = .N), by = splitBy])
#       )
#     )
#     # Targets: tracks -----------------------------------------------------------------------
#     targets_tracks <- c(
#       tar_target(
#         tracks,
#         make_track(splits, long, lat, datetime, crs = crs, all_cols = TRUE),
#         pattern = map(splits)
#       ),
#       tar_target(
#         resamples,
#         resample_tracks(tracks, rate, tolerance, probsfilter = 0.95),
#         pattern = map(tracks)
#       ),
#
#       tar_target(
#         distributions,
#         ggplot(resamples, aes(sl_)) + geom_density(alpha = 0.4)
#       ),
#
#       tar_target(
#         sl_distr,
#         fit_distr(resamples$sl_, "gamma")
#       ),
#
#       tar_target(
#         ta_distr,
#         fit_distr(resamples$ta_, "vonmises")
#       ),
#       tar_target(
#         randsteps,
#         make_random_steps(resamples, sl_distr, ta_distr),
#         pattern = map(resamples)
#       ),
#
#       tar_target(
#         distparams,
#         calc_distribution_parameters(randsteps),
#         pattern = map(randsteps)
#       ),
#
#       tar_target(
#         dattab,
#         make_data_table(randsteps)
#       ),
#
#       tar_target(
#         addyear,
#         dattab[, `:=`(
#           year = lubridate::year(t2_),
#           int.year = plyr::round_any(lubridate::year(t2_), interval, floor)
#         )]
#       ),
#
#
#       # create step ID across individuals
#     tar_target(
#       stepID,
#       addyear[,indiv_step_id := paste(id, step_id_, sep = "_") ]
#     ),
#
#
#       tar_target(
#         buffer,
#         plyr::round_any(median(addyear$sl_, na.rm = T), sl.interval, floor)
#       )
#     )
#     c(targets_prep, targets_tracks)
# ')
#
#   # write the dynamic targets script to the store_path
#   if (!dir.exists(store_path)) {
#     dir.create(store_path, recursive = TRUE)
#   }
#   writeLines(script_text, script_path)
#
#   message("Targets script written to: ", script_path)
#   message("Module R path: ", module_r_path)
#   message("Store path: ", store_path)
#
#   # Run the pipeline reproducibly
#   tar_make(script = script_path, callr_function = NULL, store = targets::tar_config_get("store"))
#
#
#   #save targets objects into the simlist
#   sim$tracks <- tar_read(stepID)
#   sim$distparams <- tar_read(distparams)
#   sim$buffer <- tar_read(buffer)
#
#   return(sim)
# }
.inputObjects <- function(sim) {

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  #dpath should be in the project folder not the module folder, Ask eliot about
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  return(invisible(sim))
}
