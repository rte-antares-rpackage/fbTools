#' Project on direction
#' 
#' 
#' @param PT {data.table} Point for direction to project
#' @param PTDF {data.frame | data.table}
#' @param step {numeric} manimum step (MW)
#'
#' @examples
#' 
#' \dontrun{
#' 
#'  CP = data.table()
#'  CP$AT <- CP$DE <- CP$FR <- CP$NL <- 0
#'  CP$BE <- 1
#'  PROJ <- getProjPT(CP, PTDF)
#' 
#' }
#'
#' @export
getProjPT <- function(PT, PTDF, step = 5){
  coefD <- PT/(max(abs(PT)))*step
  CP2 <- coefD
  k <- rep(0, nrow(PTDF))
  while(all(k<PTDF$ram)){
    PP <- PTDF[, .SD, .SDcols = grep(pattern = "ptdf", names(PTDF))]
    for(i in 1:ncol(PP)){
      k <- k + unlist(PP[, .SD, .SDcols = i]) * unlist(coefD[, .SD, .SDcols = i])
    }
    CP2 <- CP2 + coefD
  }
  CP2 <- CP2 - coefD
  CP2
}

