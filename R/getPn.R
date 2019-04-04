#' getNP from net position data
#'
#' @param file {character} net position file(s)
#'
#' @export
getNP <- function(file){
  rbindlist(sapply(file, function(X){
    
    X <- xmlToList(xmlParse(X))
    PN <- Reduce(merge, lapply(X[names(X)=="AccountTimeSeries"], function(ZZ){
      areaName <- .ctrlAreaName(ZZ$Area["v"])
      tsPer <- ZZ$Period
      timeInt <- .getTime(tsPer$TimeInterval)
      
      extr <- Reduce(rbind, lapply(tsPer[names(tsPer)=="AccountInterval"], function(X){
        inD <- as.numeric(X$InQty)
        outD <- as.numeric(X$OutQty)
        c(ifelse(inD != 0, -inD, outD), as.numeric(X$Pos))
      }))
      
      dt <- data.table(timestamp = timeInt, CT = extr[,1], hh = extr[,2])
      dt$timestamp <- dt$timestamp + (dt$hh-1)*3600
      dt[, hh := NULL]
      setnames(dt, "CT", areaName)
      dt
    }))
    ColO <- order(names(PN)[2:ncol(PN)]) + 1
    PN[, .SD, .SDcols = c(1, ColO)]
  }, simplify = FALSE))
}




.ctrlAreaName <- function(area){
  if(area == "10YFR-RTE------C")return("FR")
  if(area == "10YBE----------2")return("BE")
  if(area == "10YAT-APG------L")return("AT")
  if(area == "10YNL----------L")return("NL")
  return("DE")
  
}
