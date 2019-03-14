#' Add a PTDF to a data.table
#'
#' @param PTDF {data.frame | data.table}
#' @param ctr {list} ptdf of new contrain, named list
#' @param ram  {numeric} ram  of new contrain
#' @param name  {character} name of new contrain.
#'
#' @examples
#' \dontrun{
#' PLAN2 <- addConstrain(PLAN, ctr = list(AT = 0, BE = 1, DE = 0, FR = 0, NL = 0),
#' ram = -1000 )
#' }
#'
#' @export
addConstrain <- function(PTDF, ctr, ram , name = "CustomConstrain"){
  namCT <- paste0("ptdf", names(ctr))
  sapply(namCT, function(Z){
    if(!Z%in%names(PTDF))stop("Error in ctr name you must specify country name in camel case without 'ptdf' before")
  })
  names(ctr) <- paste0("ptdf",  names(ctr) )


  rbindlist(sapply(unique(PTDF$timestamp), function(X){
    DD <- PTDF[timestamp == X]

    newrax <- data.table(emp = 0)
    for(i in namCT){
      newrax[[i]] <- ctr[[i]]
    }
    nm <- names(DD)
    newrax$emp <- NULL
    newrax$ram <- ram
    newrax$timestamp <- X
    newrax$name <- name
    rbindlist(list(DD, newrax), fill = TRUE)
  }, simplify = FALSE))

}
