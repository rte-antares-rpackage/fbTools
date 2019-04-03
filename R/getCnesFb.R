#' Extract ptdf from cnes XML
#'
#' @param file {character} XML file
#' @param nbcl {numeric} parralel process
#' @param sizechunk {numeric} size of memory chunk.
#'
#' @examples
#' \dontrun{
#'    getCnesFb("fb.XML", 4 )
#' }
#'
#' @import XML data.table
#' @export
getCnesFb <- function(file, nbcl, sizechunk = 10)
{

  file <- split(file, ceiling(seq_along(file)/(sizechunk)))

  oud <- rbindlist(lapply(file, function(ffl){

    cl <- makeCluster(nbcl)
    clusterEvalQ(cl, library("XML"))
    clusterEvalQ(cl, library("data.table"))
    end <- rbindlist(parSapply(cl, ffl, function(fl){
      
      PLAN <- xmlParse(fl)

      ns <- c("a" = xmlNamespaceDefinitions(PLAN, simplify = TRUE))

      ##Time
      outT <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:TimeInterval', ns,
                         addFinalizer = FALSE)
      out <- xmlSApply(outT, xmlAttrs)
      tim <- fbTools:::.getTime(out)
      rm(outT)

      out <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult', ns,
                        addFinalizer = FALSE)
      endAll <- rbindlist(lapply(out, function(DD){

        ot <- xmlElementsByTagName(DD, "ptdfs")$ptdfs
        if(is.null(ot))return(data.table(DDDDD = 0))
        oot <- xmlElementsByTagName(ot, "ptdf")
        PT <- unlist(lapply(oot, function(D)
        {
          ct <- xmlElementsByTagName(D, "hub")$hub
          xmlAttrs(ct)
        }))
        vl <- as.numeric(xmlSApply(ot, xmlValue))
        names(vl) <- paste0("ptdf", PT)
        data.table(t(vl))

      }), use.names = TRUE ,fill = TRUE )
      rm(out)

      if('DDDDD'%in%names(endAll))endAll$DDDDD <- NULL
      RAM2 <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:ram', ns,
                         addFinalizer = FALSE)
      RAM <- as.numeric(xmlSApply(RAM2, xmlValue))
      endAll[, ram := RAM]
      rm(RAM2)

      FMAX2 <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:fMax', ns,
                          addFinalizer = FALSE)
      FMAX <- as.numeric(xmlSApply(FMAX2, xmlValue))
      endAll[, fMax := FMAX]
      rm(FMAX2)


      fref2 <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:fRef', ns,
                          addFinalizer = FALSE)
      fref <- as.numeric(xmlSApply(fref2, xmlValue))
      endAll[, fref := fref]
      rm(fref2)


      amr <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:amr', ns,
                          addFinalizer = FALSE)
      amr <- as.numeric(xmlSApply(amr, xmlValue))
      endAll[, amr := amr]

      ltaMargin <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:ltaMargin', ns,
                        addFinalizer = FALSE)
      ltaMargin <- as.numeric(xmlSApply(ltaMargin, xmlValue))
      endAll[, ltaMargin := ltaMargin]

      frm <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:frmMw', ns,
                              addFinalizer = FALSE)
      frm <- as.numeric(xmlSApply(frm, xmlValue))
      endAll[, frm := frm]

      fav <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:fav', ns,
                        addFinalizer = FALSE)
      fav <- as.numeric(xmlSApply(fav, xmlValue))
      endAll[, fav := fav]


      tsoOrigin <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:criticalBranch/a:tsoOrigin', ns,
                        addFinalizer = FALSE)
      tsoOrigin <- xmlSApply(tsoOrigin, xmlValue)
      endAll[, tsoOrigin := tsoOrigin]


      lim2 <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult/a:domainLimit/a:region', ns,
                         addFinalizer = FALSE)
      lim <- xmlSApply(lim2, xmlValue)
      lim <- ifelse(lim == "true", TRUE, FALSE)
      endAll[, presolve := lim]
      rm(lim2)

      branch2 <- getNodeSet(PLAN, '/a:FlowBasedDomainDocument/a:FlowBasedDomainTimeSeries/a:Period/a:Interval/a:FlowBasedDomain/a:constraintResults/a:constraintResult', ns,
                            addFinalizer = FALSE)
      branch <- xmlSApply(branch2, xmlAttrs)
      rm(branch2)
      branch <- t(branch)
      endAll <- cbind(endAll, data.table(branch))
      endAll <- endAll[!c(is.na(endAll[, .SD, .SDcols = 1]))]
      endAll[, timestamp := tim]
      free(PLAN)
      endAll
    }, simplify = FALSE))
    stopCluster(cl)
    end
  }))


}


.getTime <- function(tim){
  tim <- substr(tim, 1, 16)
  tim <-  as.POSIXct(tim, format = "%Y-%m-%dT%H:%M", tz = "UTC")
  attr(tim, "tzone") <- "CET"
  tim
}
