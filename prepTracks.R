defineModule(sim, list(
  name = "prepTracks",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(prepTracks = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "prepTracks.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9003)", "ggplot2", "targets", "tarchetypes", "amt", "data.table",
                  "terra", "sf", "sp", "ggplot2", "distanceto", "glmmTMB", "dtplyr"),
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
    #raster stack?
    #move params
    createsOutput(objectName = "extrctedDat", objectClass = NA, desc = NA)
    
  )
))

doEvent.prepTracks = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  browser()
  #attempt 1 work but its not reproducible
  # tarfile <- paste(getOption("spades.modulePath"), "/prepTracks/_targets/_targets.R", sep = "")
  # setwd(paste(getOption("spades.modulePath"), "/prepTracks/_targets", sep = ""))
  #tar_config_set(script = tarfile)
  
  #move the _targets.R to the projectPath
  file.copy(paste(getOption("spades.modulePath"), "/prepTracks/_targets/_targets.R", sep = ""), getOption("spades.projectPath"), overwrite = FALSE)
  #move the R files
  dir.create("R")
  file.copy(paste(getOption("spades.modulePath"), "/prepTracks/_targets/R", sep = ""), paste(getOption("spades.modulePath"), "R", sep = ""), overwrite = FALSE, recursive = TRUE)
  st_write(sim$caribouLoc, "/outputs/caribouLoc.shp")
  tar_make()
  #test this
  #tar_make(callr_function = NULL, use_crew = FALSE, as_job = FALSE)

  return(invisible(sim))
}
.inputObjects <- function(sim) {

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  #dpath should be in the project folder not the module folder, Ask eliot about 
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  #the data is going to have to be stored somewhere, dpath is going to have to be in the targets folder
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

