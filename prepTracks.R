defineModule(sim, list(
  name = "prepTracks",
  description = "A targets pipeline to prepare tracks derived from location data",
  keywords = c("Targets", "Random steps", "Movement Characteristics"),
  authors = c(person("Julie", "Tuner", email = "", role = c("aut", "cre")),
              person("Rory", "McInnes", email = "rory_mcinnes@hotmail.com", role = c("aut", "cre"))),
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
                    "Should caching of events or module be used?"),
    defineParameter("outputFolderID", "character", "https://drive.google.com/drive/folders/1CRSY_tJucL3E8VDgUYEv9WXuG1nKImH3", NA, NA,
                    "Google Drive folder ID for workflow outputs")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "caribouLoc", objectClass = "data.table",
                 desc = "Harmonized and cleaned caribou locations of all jurisdictions provided")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "tracks", objectClass = "data.table",
                  desc = "Prepared movement tracks with random steps and movement characteristics"),
    createsOutput(objectName = "distparams", objectClass = "list",
                  desc = "A list of parameters from a fitted distribution"),
    createsOutput(objectName = "buffer", objectClass = "numeric",
                  desc = "Buffer for moving windows based on the movement characteristics")

  )
))

doEvent.prepTracks = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # run the targets pipeline that generates the tracks
      sim <- scheduleEvent(sim, time(sim), "prepTracks", "preparingTracks")
      sim <- scheduleEvent(sim, time(sim), "prepTracks", "save")

    },
    preparingTracks = {
      if (is.null(sim$caribouLoc)){
        stop("The object caribouLoc is NULL. Please debug.")
      }
      # run the targets pipeline script
      tracksReady <- prepareTracks(sim)

    },

    save = {
      message("Saving distparams to workflowOutputs on Google Drive")

      if (!requireNamespace("googledrive", quietly = TRUE)) {
        stop("Package 'googledrive' is required for this event.")
      }

      folder_id <- Par$outputFolderID

      if (is.na(folder_id)) {
        stop("outputFolderID parameter must be supplied")
      }

      output_folder <- googledrive::as_id(folder_id)

      tmp_file <- tempfile(fileext = ".rds")

      # Save distparams
      distparams <- sim$distparams

      saveRDS(distparams, tmp_file)

      message("Uploading distparams")

      googledrive::drive_upload(
        media = tmp_file,
        path = output_folder,
        name = paste0("distparams", Par$.studyAreaName, ".rds"),
        overwrite = TRUE
      )

      unlink(tmp_file)

      message("Upload complete → caribouWorkflow/workflowOutputs")
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  #dpath should be in the project folder not the module folder, Ask eliot about
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  return(invisible(sim))
}
