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
#'  CP$AT <- CP$DE <- CP$FR <- CP$NL <- 0
#'  CP$BE <- 1
#'  PROJ <- projectionOnDir(CP, domain)
#' 
#' }
#'
#' @export
projectionOnDir <- function(domain, point, step = 5){
  coefD <- point/(max(abs(point)))*step
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

