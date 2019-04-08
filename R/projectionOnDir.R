#' Project on direction
#' 
#' @param domain {data.frame | data.table}
#' @param point {data.table} Point for direction to project
#' @param step {numeric} manimum step (MW)
#'
#' @examples
#' 
#' \dontrun{
#' 
#'  CP = data.table()
#'  CP$NP_AT <- CP$NP_DE <- CP$NP_FR <- CP$NP_NL <- 0
#'  CP$NP_BE <- 1
#'  PROJ <- projectionOnDir(CP, domain)
#' 
#' }
#'
#' @export
projectionOnDir <- function(domain, point, step = 5){
  coefD <- point/(max(abs(point)))*step
  names(point) <- gsub("NP_", "", names(point))
  CP2 <- coefD
  k <- rep(0, nrow(domain))
  while(all(k<domain$ram)){
    PP <- domain[, .SD, .SDcols = grep(pattern = "ptdf", names(domain))]
    for(i in 1:ncol(PP)){
      k <- k + unlist(PP[, .SD, .SDcols = i]) * unlist(coefD[, .SD, .SDcols = i])
    }
    CP2 <- CP2 + coefD
  }
  CP2 <- CP2 - coefD
  CP2
}

