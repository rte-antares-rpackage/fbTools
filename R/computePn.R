#' Extract ptdf from cnes XML
#'
#' @param LINK {data.table} LINK data.table, first column is timestamp others are area1-area2
#' @param ctry {character} areas for compute net position.
#' @param name {character} prefix for area.
#'
#' @examples
#' \dontrun{
#' 
#'    refProf = getRefProgs("D:/Users/titorobe/Desktop/CNESdata/Refprogs/22XCORESO------S_17XTSO-CS------W_CWE-FB-D2CF-100_20180903F10101.xml")
#'    computePn(refProf, c("BE", "NL", "DE", "FR", "APG"), "CWEP")
#'    
#' }
#'
#' @import XML data.table
#' @export
computePn <- function(LINK, ctry, name = ""){
  allCt <- sapply(ctry, function(ct){
    ddn <- unlist(lapply(strsplit(  names(LINK),"-"), function(X){
      any(ct==X) & any(ctry%in%X)
    }))
    LLCT <- LINK[,.SD, .SDcols =   c(1, which(ddn))]
    LKout <- lapply(strsplit(names(LLCT),"-"), function(X){
      if(X[1] == "timestamp")return(LLCT[, .SD, .SDcols = X])
      if(X[1]==ct)return(LLCT[, .SD, .SDcols = paste(X, collapse = "-")])
      if(X[2]==ct){

        TP <- -LLCT[, .SD, .SDcols = paste(X, collapse = "-")]
        names(TP) <-  paste(rev(X), collapse = "-")
        return(TP)
      }
    })

    LKout <- Reduce(cbind, LKout)
    rfS <- rowSums(LKout[, .SD, .SDcols = 2:ncol(LKout)])
    REEND <- data.table(timestamp = LKout$timestamp, V1 = rfS)
    names(REEND)[2] <- ct
    REEND
  }, simplify = FALSE)
  allCt <- Reduce(merge, allCt)
  names(allCt)[2:ncol(allCt)] <- paste0(names(allCt)[2:ncol(allCt)], name)
  allCt
}
