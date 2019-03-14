#' Extract RefProgs from XML
#'
#' @param file {character} XML file(s)
#'
#' @examples
#' \dontrun{
#'    getRefProgs("fb.XML")
#' }
#'
#' @import XML data.table
#' @export
getRefProgs <- function(file){
  rbindlist(sapply(file, function(xmlLt){
    xmlLt <- xmlToList(xmlParse(xmlLt))
    timeInt <- .getTime(xmlLt$PublicationTimeInterval)

    extr <- rbindlist(lapply(xmlLt[names(xmlLt)=="PublicationTimeSeries"], function(X){
      rbindlist(lapply(X$Period[names(X$Period)=="Interval"], function(Y){
        inD <- as.numeric(Y$Qty)
        Pos <- as.numeric(Y$Pos) - 1
        data.table(X$TimeSeriesIdentification,  inD, Pos)
      }))
    }))
    if(nrow(extr)==0)return(NULL)
    extr$timestamp <- timeInt + extr$Pos*3600-1800
    extr$Pos <- NULL
    link <- dcast(timestamp~V1, data=extr, value.var = "inD")
    names(link) <- gsub("[+]","",names(link))
    link

  }, simplify = FALSE))
}

