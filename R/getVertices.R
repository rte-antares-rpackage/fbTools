#' vertices from PTDF data
#'
#' @param PTDF {data.frame | data.table}
#' @param ctrdel {character} name of country deleted
#'
#'
#' @examples
#' \dontrun{
#'   PTDFv <- getVertices(PTDF, "NL")
#' }
#' @import vertexenum
#' @export
getVertices <- function(PTDF,  ctrdel = "NL"){
  PTDF <- data.table(PTDF)

  DDout <- sapply(unique(PTDF$timestamp), function(X){
    DD <- .foundVertices(PTDF[timestamp == X], ctrdel = ctrdel)
    DD$timestamp <- X
    DD
  }, simplify = FALSE)
  end <- rbindlist(DDout)

  end
}


.foundVertices <- function(PTDF, ctrdel = "NL"){

  ctry <- names(PTDF)[grep("ptdf", names(PTDF))]
  ctrdel <- paste0("ptdf", ctrdel)
  ctrnodel <- ctry[ctry!=ctrdel]

  for(i in ctrnodel){
    PTDF[[i]] <- PTDF[[i]] - PTDF[[ctrdel]]
  }
  vertices <- vertexenum::enumerate.vertices(as.matrix(PTDF[,.SD, .SDcols = ctrnodel]), PTDF$ram)
  vertices <- data.table(vertices)
  names(vertices) <- ctrnodel
  vertices
}



