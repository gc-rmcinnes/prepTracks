#' @title calculate time since some variable
#' @export
#' @author Julie W. Turner
#' 
calc_ts <- function(DT, var, where  = 'end', no.data=100){
  
  if(where == 'start'){
    newvar <- paste('ts', var, 'start', sep = '_')
    oldvar <- paste(var, 'start', sep = '_')
    DT[,paste('ts', var, 'start', sep = '_') := year - get(oldvar)]
    DT[get(newvar)<0, paste('ts', var, 'start', sep = '_'):=no.data]
    DT[is.na(get(newvar)), paste('ts', var, 'start', sep = '_'):=no.data]
  }
  
  if (where == 'end') {
    newvar <- paste('ts', var, 'end', sep = '_')
    oldvar <- paste(var, 'end', sep = '_')
    DT[,paste('ts', var, 'end', sep = '_') := year - get(oldvar)]
    DT[get(newvar)<0, paste('ts', var, 'end', sep = '_'):=no.data]
    DT[is.na(get(newvar)), paste('ts', var, 'end', sep = '_'):=no.data]
  }
  
  if (where == 'both') {
    newvar_start <- paste('ts', var, 'start', sep = '_')
    oldvar_start <- paste(var, 'start', sep = '_')
    newvar_end <- paste('ts', var, 'end', sep = '_')
    oldvar_end <- paste(var, 'end', sep = '_')
    DT[,paste('ts', var, 'start', sep = '_') := year - get(oldvar_start)]
    DT[,paste('ts', var, 'end', sep = '_') := year - get(oldvar_end)]
    
    DT[get(newvar_start)<0, paste('ts', var, 'start', sep = '_'):=no.data]
    DT[get(newvar_end)<0, paste('ts', var, 'end', sep = '_'):=no.data]
    DT[is.na(get(newvar_start)), paste('ts', var, 'start', sep = '_'):=no.data]
    DT[is.na(get(newvar_end)), paste('ts', var, 'end', sep = '_'):=no.data]
  }
  
 return(DT)
  
}