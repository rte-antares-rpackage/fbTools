#' 3D flowbased plot
#'
#' This function generates 3D flowbased plot
#'
#' @param df {data.frame} vertices
#' @param grouping {character} column for dates
#' @param country {character} 3 country (for dims)
#' @param legend {boolean} add legend
#' @param alpha {numeric} transparancy
#'
#' @import   XML httpuv httr data.table pipeR geometry rgl scales
#' @export
hullPlot <- function(df, grouping, country = c("NP_DE", "NP_BE", "NP_FR"), legend = TRUE, alpha = 1){
  df <- data.frame(df)
  # requires that the "grouping" column is printed as per the name of the column
  # you want separate hulls for
  #country <- paste0("ptdf",country )

  allSites <- sort(as.vector(unique(df[[grouping]])))
  matList <- list()
  hullList <- list()
  cols <- as.character(brewer_pal(type = "qual", palette = 1, direction = 1)(length(allSites)))


  # this loop creates the points for each site
  # it also calculates a separate hull for each site
  # "site" or whatever grouping variable you are using
  for(thisSite in 1:length(allSites)){

    tmp <- df[df[grouping] == allSites[thisSite], ]
    tmp <- data.table(tmp)
    plot3d(tmp[[country[1]]], tmp[[country[2]]], tmp[[country[3]]], col = cols[thisSite], box = FALSE,
           type = "s", radius = 0.03, add = ifelse(thisSite > 1, TRUE, FALSE),
           xlab = country[1], ylab = country[2], zlab = country[3],
           xlim = range(tmp[[country[1]]]),
           ylim = range(tmp[[country[2]]]),
           zlim = range(tmp[[country[3]]])
    )

    matList[[thisSite]] <- matrix(
      c(tmp[[grep(country[1], names(tmp))]],
        tmp[[grep(country[2], names(tmp))]],
        tmp[[grep(country[3], names(tmp))]]), ncol = 3)

    hullList[[thisSite]] <- t(convhulln(matList[[thisSite]]))

  }


  # this will run if you have legend = TRUE (the default)
  if(legend){
    # this slows down the plotting which is necessary otherwise
    # the printing can lag and the legend goes to a funny size
    Sys.sleep(0.5)

    # you can change your legend as per 'legend' commands
    legend3d("bottom", legend = allSites,
             # uses the same cols as for plotting
             col = cols,
             # symbol size
             pch = 16,
             inset=c(0.02),
             horiz = TRUE)

  }


  # this loop plots the hulls
  for(hull in seq_along(matList)){
    rgl.triangles(matList[[hull]][hullList[[hull]],1],
                  matList[[hull]][hullList[[hull]],2],
                  matList[[hull]][hullList[[hull]],3],
                  col=cols[hull],

                  # change the alpha to change how see through they are
                  alpha=alpha)

  }

}
