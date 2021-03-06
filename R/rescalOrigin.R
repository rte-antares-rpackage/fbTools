#' Rescale data
#'
#' @param domain {data.frame | data.table}
#' @param point {data.table} Point for rescale.
#' 
#' @examples
#' \dontrun{
#'  PT = data.table()
#'  PT$NP_AT <- PT$NP_DE <- PT$NP_FR <- PT$NP_NL <- 0
#'  PT$NP_BE <- 1
#'  PT$timestamp  <- '2018-03-03 15:00:00'
#'  PROJ <- shiftOriginDomain(domain, PT)
#' }
#'
#'
#' @export
shiftOriginDomain <- function(domain, point){
  CT <- names(point)
  CT <- CT[!CT=="timestamp"]
  names(PT) <- gsub("NP_", "", names(PT))
  if(!all(unique(domain$timestamp)%in%point$timestamp)){
    stop("Miss timestamp in CT")
  }

  domain <- merge(point, domain, by = "timestamp")
  domain$newRam = domain$ram
  for(i in CT){

    domain$newRam <- domain$newRam - unlist(domain[, .SD, .SDcols = i]*domain[, .SD, .SDcols = paste0("ptdf",i)])
    domain[, eval(i) := NULL]
  }
  domain$ram <- domain$newRam
  domain[,newRam := NULL]
  domain
}
