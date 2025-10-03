# === Targets workflow: iSSA with amt -------------------------------------
# Julie Turner
# adapted from code by Alec L. Robitaille


# Packages ----------------------------------------------------------------
library(targets)

library(tarchetypes)
library(amt)
library(data.table)
library(terra)
library(sf)
library(sp)
library(ggplot2)
#library(maptools)
library(glmmTMB)
library(distanceto)
library(dtplyr)
# library(dplyr, warn.conflicts = FALSE)

#library(landscapemetrics)
browser()
# Functions ---------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)
#source('R/functions.R')


# Options -----------------------------------------------------------------
tar_option_set(format = 'qs', 
               workspace_on_error = T,
               error = "null")


# Variables ---------------------------------------------------------------
set.seed(37)
#file structure may change
path <- file.path('outputs', 'caribouLoc.shp')
#canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
#studyArea <- file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp')


prop_water <- file.path('data', 'raw-data', 'prop_land', 'prop_water')
prop_snow <- file.path('data', 'raw-data', 'prop_land', 'prop_snow')
prop_rock <- file.path('data', 'raw-data', 'prop_land', 'prop_rock')
prop_barren <- file.path('data', 'raw-data', 'prop_land', 'prop_barrenland')
prop_bryoids <- file.path('data', 'raw-data', 'prop_land', 'prop_bryoids')
prop_shrub <- file.path('data', 'raw-data', 'prop_land', 'prop_shrub')
prop_wetland <- file.path('data', 'raw-data', 'prop_land', 'prop_wetland')
prop_wettreed <- file.path('data', 'raw-data', 'prop_land', 'prop_wet_treed')
prop_herbs <- file.path('data', 'raw-data', 'prop_land', 'prop_herbs')
prop_needleleaf <- file.path('data', 'raw-data','prop_land', 'prop_needleleaf')
prop_deciduous <- file.path('data', 'raw-data', 'prop_land', 'prop_deciduous')
prop_mixed <- file.path('data', 'raw-data', 'prop_land', 'prop_mixed')



linfeat <- file.path('data', 'raw-data', 'wbi_road_rail.shp')
fires <- file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires')

linfeat_other_2010 <- file.path('data', 'raw-data', 'ECCC_disturbance', 'WB_lfother_2010.shp')
linfeat_other_2015 <- file.path('data', 'raw-data', 'ECCC_disturbance', 'WB_lfother_2015.shp')

disturb_2010 <- file.path('data', 'raw-data', 'ECCC_disturbance', 'WB_disturb_other_2010.tif')
disturb_2015 <- file.path('data', 'raw-data', 'ECCC_disturbance', 'WB_disturb_other_2015.tif')

harv <- file.path('data', 'raw-data', 'WB_harv_1985-2020.tif')


id <- 'id'
datetime <- 'datetime'
longlat = FALSE
#not actually longitude and latitude, just don't want to change code
long <- 'x'
lat <- 'y'
crs <- st_crs(3978)$wkt

# minimum year we want to pull data for
minyr <- 2010
maxyr <- 2021


# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
splitBy <- id
interval <- 5 # to round by 5 year intervals
sl.interval <- 50 # to round by 50m intervals



# Resampling rate 
rate <- hours(13)

# Tolerance
tolerance <- minutes(150)



# Targets: prep -----------------------------------------------------------------
targets_prep <- c(
  # Read input data, change this to st_read
  tar_target(
    input,
    readRDS(path)
  ),
  
  # Remove duplicated and incomplete observations
  tar_target(
    mkunique,
    make_unique_complete(input, id, datetime, long, lat)
  ),
  
  
  # # load linear features
  # tar_target(
  #   lf,
  #   load_sf(linfeat, crs)
  # ),
  # 
  # # load other linear features
  # tar_target(
  #   lf_other_2010,
  #   load_sf(linfeat_other_2010, crs)
  # ),
  # 
  # # load other linear features
  # tar_target(
  #   lf_other_2015,
  #   load_sf(linfeat_other_2015, crs)
  # ),
  
  # subsample data to that greater than minimum year
  tar_target(
    subdt,
    mkunique[lubridate::year(datetime)>= minyr]
  ),
  
  # Set up split -- these are our iteration units
  tar_target(
    splits,
    subdt[, tar_group := .GRP, by = splitBy],
    iteration = 'group'
  ),
  
  tar_target(
    splitsnames,
    unique(subdt[, .(path = path), by = splitBy])
  )
)


# Targets: tracks -----------------------------------------------------------------------
# Make tracks. Note from here on, when we want to iterate use pattern = map(x)
#  where x is the upstream target name
targets_tracks <- c(
  tar_target(
    tracks,
    make_track(splits, long, lat, datetime, crs = crs, all_cols = T),
    pattern = map(splits)
  ),
  
  # Resample sampling rate, filtering out extra long steps
  tar_target(
    resamples,
    resample_tracks(tracks, rate, tolerance, probsfilter = 0.95),
    pattern = map(tracks)
  ),
  
  # # check for extra long steps
  # tar_target(
  #   quantiles,
  #   quantile(resamples$sl_, probs = c(0.95, 0.99), na.rm = T)
  # ), 
  # 
  # # remove extra long steps
  # tar_target(
  #   resamples2,
  #   filter(resamples, sl_<=plyr::round_any(quantiles[['99%']], 50, floor))
  # ),
  
  # Check step distributions
  #  iteration = 'list' used for returning a list of ggplots,
  #  instead of the usual combination with vctrs::vec_c()
  tar_target(
    distributions,
    ggplot(resamples, aes(sl_)) + geom_density(alpha = 0.4)#,
    # pattern = map(resamples),
    # iteration = 'list'
  ),
  
  
  # create random steps and extract covariates
  # after `distributions` target
  
  # calc global sl distribution
  tar_target(
    sl_distr,
    fit_distr(resamples$sl_, 'gamma')
  ),
  
  # calc global ta distribution
  tar_target(
    ta_distr,
    fit_distr(resamples$ta_, 'vonmises')
  ),
  
  # create random steps and extract covariates
  tar_target(
    randsteps,
    make_random_steps(resamples, n_rand = 10, sl_distr, ta_distr),
    pattern = map(resamples)
  ),
  
  
  
  # # Distribution parameters
  # tar_target(
  #   distparams,
  #   calc_distribution_parameters(randsteps),
  #   pattern = map(randsteps)
  # ), 
  # 
  # # make a data.table so easier to manipulate
  # tar_target(
  #   dattab,
  #   make_data_table(randsteps)
  # ),
  # 
  # # add a year column
  # tar_target(
  #   addyear,
  #   dattab[,`:=`(year=lubridate::year(t2_), 
  #                int.year=plyr::round_any(lubridate::year(t2_), interval, floor))]
  # )
)


# # Targets: extract ------------------------------------------------------------------
# ## Proportion of land ----
# 
# targets_propland <- c(
#   tar_target(
#     buffer,
#     plyr::round_any(median(addyear$sl_, na.rm = T), sl.interval, floor)
#   ),
#   
#   # # can't force this to run before needing to extract from it for some reason...
#   # tar_target(
#   #   make_propland,
#   #   make_landforest_prop(studyArea, crs, buff = buffer, startyr = minyr, endyr = 2019)
#   # ), 
#   # 
#   tar_target(
#     extr_propwater,
#     extract_by_year(addyear, var = prop_water, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_propsnow,
#     extract_by_year(extr_propwater, var = prop_snow, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_proprock,
#     extract_by_year(extr_propsnow, var = prop_rock, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_propbarren,
#     extract_by_year(extr_proprock, var = prop_barren, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_propbryoids,
#     extract_by_year(extr_propbarren, var = prop_bryoids, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_propshrub,
#     extract_by_year(extr_propbryoids, var = prop_shrub, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_propwetland,
#     extract_by_year(extr_propshrub, var = prop_wetland, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_propwettreed,
#     extract_by_year(extr_propwetland, var = prop_wettreed, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_propherbs,
#     extract_by_year(extr_propwettreed, var = prop_herbs, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_propneedle,
#     extract_by_year(extr_propherbs, var = prop_needleleaf, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_propdecid,
#     extract_by_year(extr_propneedle, var = prop_deciduous, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   ),
#   
#   tar_target(
#     extr_propmixed,
#     extract_by_year(extr_propdecid, var = prop_mixed, startyr = minyr, endyr = 2019, maxyr, where = 'both')
#   )
# )
# 
# 
# 
# ## time since ----
# targets_timesince <- c(    
#   # Extract harvest
#   tar_target(
#     extrharv,
#     extract_pt(extr_propmixed, harv, 'harv', where = 'both')
#   ),
#   
#   # calculate time since harvest
#   tar_target(
#     tsharv,
#     calc_ts(extrharv, var = 'harv', where = 'both', no.data=100) 
#   ),
#   
#   # Extract fires
#   tar_target(
#     extrfires,
#     extract_by_year(tsharv, fires, startyr = minyr, endyr =2020, maxyr = maxyr, where = 'both')
#   ),
#   
#   # calculate time since fire
#   tar_target(
#     tsfire,
#     calc_ts(extrfires, var = 'fires', where = 'both', no.data=100) 
#   )
# )
# 
# ## distance to linear features ---- 
# ### permanent roads and rail ----
# targets_disttolf <- c(     
#   # Calculate distance to linear features
#   tar_target(
#     disttolf_2010,
#     extract_distto(tsfire, lf, 'lf', where = 'both', crs, int.yr = 2010)
#   ),
#   
#   tar_target(
#     disttolf_2015,
#     extract_distto(tsfire, lf, 'lf', where = 'both', crs, int.yr = 2015)
#   ),
#   
#   tar_target(
#     disttolf_2020,
#     extract_distto(tsfire, lf, 'lf', where = 'both', crs, int.yr = 2020)
#   )
#   
# )
# 
# targets_disttolf_combo <- c(  
#   tar_combine(
#     extrdisttolf,
#     list(targets_disttolf),
#     command = dplyr::bind_rows(!!!.x)
#   )
# )
# 
# ### other linear features ----
# targets_disttolfother <- c(     
#   
#   # Calculate distance to linear features other
#   tar_target(
#     disttolfother_2010,
#     extract_distto(extrdisttolf, lf_other_2010, 'lf_other', where = 'both', crs, int.yr = 2010)
#   ),
#   
#   tar_target(
#     disttolfother_2015,
#     extract_distto(extrdisttolf, lf_other_2015, 'lf_other', where = 'both', crs, int.yr = 2015)
#   ),
#   
#   tar_target(
#     disttolfother_2020,
#     extract_distto(extrdisttolf, lf_other_2015, 'lf_other', where = 'both', crs, int.yr = 2020)
#   )
#   
# )
# 
# targets_disttolfother_combo <- c(  
#   tar_combine(
#     extrdisttolfother,
#     list(targets_disttolfother),
#     command = dplyr::bind_rows(!!!.x)
#   )
# )
# 
# ### other disturbances ----
# targets_disturb_other <- c(     
#   # extract other disturbance
#   tar_target(
#     distother_2010,
#     extract_pt(extrdisttolfother, disturb_2010, 'disturbance', where = 'both', int.yr = 2010)
#   ),
#   
#   tar_target(
#     distother_2015,
#     extract_pt(extrdisttolfother, disturb_2015, 'disturbance', where = 'both', int.yr = 2015)
#   ),
#   
#   tar_target(
#     distother_2020,
#     extract_pt(extrdisttolfother, disturb_2015, 'disturbance', where = 'both', int.yr = 2020)
#   )
#   
# )
# 
# targets_disturb_other_combo <- c(  
#   tar_combine(
#     extrdisturbother,
#     list(targets_disturb_other),
#     command = dplyr::bind_rows(!!!.x)
#   )
# )
# 
# ## Step ID ----
# targets_stepID <- c(
#   # create step ID across individuals
#   tar_target(
#     stepID,
#     setDT(extrdisturbother)[,indiv_step_id := paste(id, step_id_, sep = '_')]
#   )
# )

# Targets: all ------------------------------------------------------------------
# Automatically grab and combine all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)
