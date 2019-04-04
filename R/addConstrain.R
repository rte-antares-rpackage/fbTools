#' Add a branch to a data.table
#'
#' @param domain {data.frame | data.table}
#' @param ptdf {list} ptdf of new contrain, named list
#' @param ram  {numeric} ram  of new contrain
#' @param name  {character} name of new contrain.
#'
#' @examples
#' \dontrun{
#' PLAN2 <- addBranch(domain,
#' ptdf = list(AT = 0, BE = 1, DE = 0, FR = 0, NL = 0),
#' ram = -1000 )
#' }
#'
#' @export
addBranch <- function(domain, ptdf, ram , name = "CustomBranch"){
  namCT <- paste0("ptdf", names(ptdf))
  sapply(namCT, function(Z){
    if(!Z%in%names(domain))stop("Error in ptdf name you must specify country name in camel case without 'ptdf' before")
  })
  names(ptdf) <- paste0("ptdf",  names(ptdf) )


  rbindlist(sapply(unique(domain$timestamp), function(X){
    DD <- domain[timestamp == X]

    newrax <- data.table(emp = 0)
    for(i in namCT){
      newrax[[i]] <- ptdf[[i]]
    }
    nm <- names(DD)
    newrax$emp <- NULL
    newrax$ram <- ram
    newrax$timestamp <- X
    newrax$name <- name
    rbindlist(list(DD, newrax), fill = TRUE)
  }, simplify = FALSE))

}
