#' getLTNom from net position data
#'
#' @param xmlLt {character} long time file
#'
#' @export
getLTNom <- function(xmlLt){
  xmlLt <- xmlToList(xmlParse(xmlLt))

  DD <- Reduce(merge, lapply(xmlLt[names(xmlLt)=="ScheduleTimeSeries"], function(ser){
    areaNameIn <- .ctrlAreaName(ser$InArea["v"])
    areaNameOut <- .ctrlAreaName(ser$OutArea["v"])
    lk <- paste0(areaNameOut, "-", areaNameIn)
    tim <- .getTime(ser$Period$TimeInterval)
    ddt <- Reduce(rbind, lapply(ser$Period[names(ser$Period)=="Interval"], function(V){
      c(as.numeric(V$Qty), as.numeric(V$Pos))
    }))
    end <- data.table(hour = tim, LT = ddt[,1], tp =  ddt[,2])
    end[,hour := hour + 3600*(tp-1)]
    end[, tp := NULL]
    setnames(end, "LT", lk)
    setnames(end, "hour", "timestamp")
    end
  }
  )
  )
  DD
  
}
