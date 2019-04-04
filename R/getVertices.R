#' vertices from PTDF data
#'
#' @param domain {data.frame | data.table}
#' @param ctrdel {character} name of country deleted
#'
#'
#' @examples
#' \dontrun{
#'   PTDFv <- getVertices(domain, "NL")
#' }
#' @import vertexenum
#' @export
getVertices <- function(domain,  ctrdel = NULL){
  domain <- data.table(domain)

  DDout <- sapply(unique(domain$timestamp), function(X){
    DD <- .foundVertices(domain[timestamp == X], ctrdel = ctrdel)
    DD$timestamp <- X
    DD
  }, simplify = FALSE)
  end <- rbindlist(DDout)

  end
}


.foundVertices <- function(PTDF, ctrdel = NULL){

  ctry <- names(PTDF)[grep("ptdf", names(PTDF))]
  if(!is.null(ctrdel))
  {
    

  ctrdel <- paste0("ptdf", ctrdel)
  ctrnodel <- ctry[ctry!=ctrdel]

  for(i in ctrnodel){
    PTDF[[i]] <- PTDF[[i]] - PTDF[[ctrdel]]
  }
  }else{
    ctrnodel = ctry
  }
  vertices <- vertexenum::enumerate.vertices(as.matrix(PTDF[,.SD, .SDcols = ctrnodel]), PTDF$ram)
  vertices <- data.table(vertices)
  names(vertices) <- ctrnodel
  vertices
}




