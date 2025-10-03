#' @title calculate time since fire
#' @export
#' @author Julie W. Turner
#' 
calc_tsf <- function(DT, where  = 'end', nofire=100){
  
  if(where == 'start'){
    DT[,tsf_start:= year - fires_start]
    DT[is.na(tsf_start), tsf_start:=nofire]
  }
  
  if (where == 'end') {
    DT[,tsf_end:= year - fires_end]
    DT[is.na(tsf_end), tsf_end:=nofire]
  }
  
  if (where == 'both') {
    DT[,`:=`(tsf_end = year - fires_end, tsf_start = year - fires_start)]
    DT[is.na(tsf_start), tsf_start:=nofire]
    DT[is.na(tsf_end), tsf_end:=nofire]
  }
  
 return(DT)
  
}