#' Rescale data
#'
#' @param PTDF {data.frame | data.table}
#' @param PT {data.table} Point for rescale.
#'
#' @export
rescalOrigin <- function(PTDF, PT){
  CT <- names(PT)
  CT <- CT[!CT=="timestamp"]

  if(!all(unique(PTDF$timestamp)%in%PT$timestamp)){
    stop("Miss timestamp in CT")
  }

  PTDF <- merge(PT, PTDF, by = "timestamp")
  PTDF$newRam = PTDF$ram
  for(i in CT){

    PTDF$newRam <- PTDF$newRam - unlist(PTDF[, .SD, .SDcols = i]*PTDF[, .SD, .SDcols = paste0("ptdf",i)])
    PTDF[, eval(i) := NULL]
  }
  PTDF$ram <- PTDF$newRam
  PTDF[,newRam := NULL]
  PTDF
}