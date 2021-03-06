#' extractPnCnes from net position data
#'
#' @param PTDF {data.table} PTDF
#' @param pos {list} eq : pos = list(AT = 0, BE = 0, DE = 0, FR = 0, NL = 0)
#' @param threshold {numeric}, threshold.
#'
#'
#' @examples
#' \dontrun{
#'    distToBranch(PTDF, pos = list(AT = 0, BE = 0, DE = 0, FR = 0, NL = 0))
#' }
#'
#' @export
distToBranch <- function(PTDF, pos, threshold = NULL){
  PT <- data.table(t(unlist(pos)))
  names(PT) <- paste0("ptdf", names(PT))
  ct <- names(PT)
  PTDF$distance <- -(as.matrix(PTDF[, .SD, .SDcols = ct])%*% t(as.matrix(PT[, .SD, .SDcols = ct]))-PTDF$ram)
  if(!is.null(threshold))return(PTDF[distance<threshold])
  PTDF
}
