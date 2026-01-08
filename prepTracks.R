defineModule(sim, list(
  name = "prepTracks",
  description = "A targets pipeline to prepare tracks derived from location data",
  keywords = "",
  authors = c(person("Julie", "Tuner", email = "", role = c("aut", "cre")),
              person("Rory", "McInnes", email = "", role = c("aut", "cre")),
              person("Micheletti", "Tati", email = "tati.micheletti@gmail.com", role = "aut")
              ),
  childModules = character(0),
  version = list(prepTracks = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "prepTracks.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9003)", "ggplot2", "amt", "data.table","fastdigest",
                  "terra", "sf", "sp", "ggplot2", 'plyr', "distanceto", "glmmTMB", "dtplyr", "glue"), #"targets", "tarchetypes", 
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
    defineParameter("crs", "character", st_crs(3978)$wkt, NA, NA,
                    "CRS to be used for the GPS points"),
    defineParameter("interval", "numeric", 5, NA, NA,
                    paste0("What is the difference in time between non-annual layers?",
                           "For ECCC disturbance, for example, we have layers every 5 years",
                           ", the default.")),
    defineParameter("sl.interval", "numeric", 50, NA, NA,
                    "ADD DESCRIPTION"),
    defineParameter("minyr", "numeric", 2010, NA, NA,
                    "ADD DESCRIPTION"),
    defineParameter("maxyr", "numeric", 2022, NA, NA,
                    "ADD DESCRIPTION"),
    defineParameter("rate", "numeric", hours(13), NA, NA,
                    "ADD DESCRIPTION"),
    defineParameter("tolerance", "numeric", minutes(150), NA, NA,
                    "ADD DESCRIPTION"),
    defineParameter("probsfilter", "numeric", 0.95, NA, NA,
                    "Probability filter for anormal steps"),
    defineParameter("aggrNonAnnualData", "character", "sequencePre", NA, NA,
                    paste0("How to aggregate non-annual layers?",
                           "'sequencePre' uses the years preceeding the layer year (that included)",
                           "'sequencePos' uses the years after the layer year (that included)",
                           "'middle' places the layer year in the middle (that included)"))
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
                  desc = "A buffer value for the step length")
  )
))

doEvent.prepTracks = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      sim <- scheduleEvent(sim, time(sim), "prepTracks", "preparingTracks")
    },
    preparingTracks = {
      if (is.null(sim$caribouLoc)){
        stop("The object caribouLoc is NULL. Please debug.")
      }
      locationsHash <- fastdigest::fastdigest(sim$caribouLoc)
      tracksReady <- Cache(prepareTracks, crs = P(sim)$crs,
                           interval = P(sim)$interval, 
                           sl.interval = P(sim)$sl.interval, 
                           minyr = P(sim)$minyr, 
                           maxyr = P(sim)$maxyr, 
                           rate = P(sim)$rate, 
                           tolerance = P(sim)$tolerance, 
                           locations = sim$caribouLoc, 
                           probsfilter = P(sim)$probsfilter,
                           locationsHash = locationsHash,
                           aggrNonAnnualData = P(sim)$aggrNonAnnualData,
                           omitArgs = "locations")
      
      sim$tracks <- tracksReady$tracks
      sim$distparams <- tracksReady$distparams
      sim$buffer <- tracksReady$buffer
      
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
