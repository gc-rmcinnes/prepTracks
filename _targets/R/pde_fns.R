#' @title Functions for PDEs
#' @export
#' @author Julie W. Turner

#' 
mod2UD <- function(modpath, envlayers, studyArea, pde.saveName = NULL, map.saveName = NULL){
  mod <- readRDS(modpath)
  mod.tab <- make_betas_tab(mod)
  pde <- make_pde(mod.tab, envlayers, saveName = pde.saveName)
  map.pde <- as.numeric(make_pde_map(pde, studyArea, saveName = map.saveName))
  return(map.pde)
}

#' prepares pde and feeds into `make_pde_map()`
make_pde <- function(mod.tab, land, saveName = NULL){
  lf.cov<- (2*as.double(mod.tab[term %like% 'distlf_end', 
                                .(estimate)])*land$log_distlf)
  lfother.cov<- (2*as.double(mod.tab[term %like% 'distlf_other_end', 
                                     .(estimate)])*land$log_distlfother)
  tsf.cov<- (2*as.double(mod.tab[term %like% 'ts_fires_end', 
                                 .(estimate)])*land$log_tsf)
  tsh.cov<- (2*as.double(mod.tab[term %like% 'ts_harv_end', 
                                 .(estimate)])*land$log_tsh)
  needleleaf.cov <- (2*as.double(mod.tab[term %like% 'needleleaf_end', 
                                         .(estimate)])*land$prop_needleleaf)
  veg.cov <- (2*as.double(mod.tab[term %like% 'veg_end', 
                                  .(estimate)])*land$prop_veg)
  mixforest.cov <- (2*as.double(mod.tab[term %like% 'mixforest_end',
                                        .(estimate)])*land$prop_mixforest)
  wets.cov <- (2*as.double(mod.tab[term %like% 'wets_end', 
                                   .(estimate)])*land$prop_wets)
  disturb.cov <- (2*as.double(mod.tab[term %like% 'disturbance_end', 
                                      .(estimate)])*land$disturb)
  
  numerator <- exp(lf.cov + lfother.cov + 
                     tsf.cov + tsh.cov + 
                     needleleaf.cov + veg.cov + 
                     mixforest.cov + 
                     wets.cov +
                     disturb.cov)
  
  
  # the normalizing constant.
  C <- global(numerator, sum, na.rm = T)
  pde <- numerator/C[[1]]
  
  if(!is.null(saveName)){
    writeRaster(pde, 
                file.path(derived, saveName), overwrite = T)
  }
  
  return(pde)
}


#' crops `make_pde()` to study area and descretizes to 10 bins
make_pde_map <- function(pde, sArea, saveName = NULL){
  pde.sa <- crop(pde, sArea, mask = T)
  plot(pde.sa)
  
  breaks <- global(pde.sa, quantile, na.rm = T, probs = seq(0,1,.1))
  v.breaks <- unname(breaks)
  t.breaks <- as.vector(t(v.breaks))
  pde.discrete <- classify(pde.sa, t.breaks, include.lowest=TRUE, brackets=TRUE)
  
  if(!is.null(saveName)){
    writeRaster(pde.discrete, 
                file.path(derived, saveName), overwrite = T)
  }
  
  return(pde.discrete)
}

load_map_layers <- function(landyr, disturbyr, ts_else){
  bryoids <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                            paste0('bryoids_500', '.tif')))
  
  shrub <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                          paste0('shrub_500', '.tif')))
  
  wet <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                        paste0('wet_500', '.tif')))
  
  wettreed <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                             paste0('wet_treed_500', '.tif')))
  
  herbs <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                          paste0('herbs_500', '.tif')))
  
  needleleaf<- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                              paste0('needleleaf_500', '.tif')))
  
  deciduous <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                              paste0('deciduous_500', '.tif')))
  
  mixed <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                          paste0('mixed_500', '.tif')))
  
  
  prop_needleleaf <- needleleaf
  prop_mixforest <- deciduous + mixed + wettreed
  prop_veg <- shrub + bryoids + herbs
  prop_wets <- wet
  print("prop land prepped")
  
  linfeat_other <- rast(file.path('data', 'raw-data', 'ECCC_disturbance', 
                                  paste0('WB_lfother_', disturbyr, '_distto.tif')))
  disturb <- rast(file.path('data', 'raw-data', 'ECCC_disturbance', 
                            paste0('WB_disturb_other_', disturbyr, '.tif')))
  
  
  
  fires <- rast(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', paste0('fires_', (disturbyr+5), '.tif')))
  
  lf.full <- rast(file.path('data', 'derived-data', 'distto_roadrail_500.tif'))
  lf <- crop(lf.full, ext(bryoids))
  harv <- rast(file.path('data', 'raw-data', 'WB_harv_1985-2020.tif'))
  
  lf_other <- resample(linfeat_other, lf, method = 'average')
  lf_other.ext <- extend(lf_other, ext(lf))
  print("linear features prepped")
  
  disturb <- resample(disturb, lf, method = 'max')
  disturb.ext <- extend(disturb, ext(lf))
  print("other anthro disturbances  prepped")
  
  harv <- resample(harv, lf, method = 'max')
  harv.ext <- extend(harv, ext(lf))
  tsh <- (disturbyr + 5) - harv.ext
  tsh[is.na(tsh)] <- ts_else
  print("harvest prepped")
  
  
  fires.crop <- resample(fires, lf, method = 'max')
  #names(land.brick) <- c("lf_dist", "lc")
  tsf <- (disturbyr + 5) - fires.crop
  tsf[is.na(tsf)] <- ts_else
  print("fires prepped")
  
  log_tsf <- log(tsf + 1)
  log_tsh <- log(tsh + 1)
  log_distlf <- log(lf + 1)
  log_distlfother <- log(lf_other.ext + 1)
  print("values transformed")
  
  
  land <- c(prop_veg, prop_needleleaf, prop_mixforest, prop_wets, log_tsf, log_tsh, log_distlf, 
            log_distlfother, disturb.ext)
  names(land) <- c('prop_veg', 'prop_needleleaf', 'prop_mixforest', 'prop_wets', 'log_tsf', 'log_tsh', 'log_distlf', 
                   'log_distlfother', 'disturb')
  return(land)
}
