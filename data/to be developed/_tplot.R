utils::globalVariables("swe_wgs84")

#' Plot function to be used in PDF
#'
#' @param x list: a list object that has been downloaded using \code{\link{occurrences}}
#' @param sweLyr string: layer of swe_WGS84 to be plotted as background. Options are =\code{c("Border" [default],
#' "Counties","LA_regions","FA_regions","Municipalities")}
#' @param main string: a title
#' @param coi string vector: list of record issues to be mapped; these can be assertion column names,
#' or 'all' or 'none' or any combination of 'error', 'warning' or 'fatal'. Column or categories in
#' your data set can be viewed using \code{check_assertions}.
#' @param pch single number or character representing point type. See description of \code{pch} in \code{\link{points}}.
#' @param cex numeric: character (or symbol) expansion. See description of \code{cex} in \code{\link{points}}.
tplot <- function(x,
                  sweLyr = "Border",
                  main,
                  coi = c("fatal", "error"),
                  pch, cex) {

  if (missing("pch")) {
    pch <- 19
  } else {
    if (length(pch) > 1) {
      pch <- pch[1]
      warning("only using first element of supplied pch vector")
    }
    if (nchar(pch) > 1) {
      pch <- substr(pch, 1, 1)
      warning("only using first character of supplied pch text")
    }
  }

  ## load swe map data
  ## note this should ideally be states
  data("swe_wgs84",
       package = "sbdi4r2",
       envir = environment())

  plot(swe_wgs84[[sweLyr]]$geometry, col="grey",
       border = ifelse(sweLyr == "Border", NA, 1)) #draw the base Sweden
  title(main = main)
  # degAxis(1); degAxis(2) #add on the axis
  points(x$longitude, x$latitude, pch = pch, col = "black")
  if (is.null(coi)) {
    legend("bottomleft", legend = "assumed good",
           pch = pch, col="black",
           bty = "n", cex = cex)
  } else {
    legend.cols <- rainbow(length(coi)) #define the legend colors
    c2use <- NULL #define columns to keep because they had issues
    for (ii in 1:length(coi)) {
      roi <- which(as.logical(x[, coi[ii]])==TRUE) #define the points that have the issue
      if (length(roi) > 0) {
        points(x$longitude[roi], x$latitude[roi], pch=pch, col=legend.cols[ii])
        c2use <- c(c2use, ii)
      }
    }
    if (is.null(c2use)) {
      legend("bottomleft", legend="assumed good",
             pch=pch, col="black", bty="n", cex=cex)
    } else {
      legend("bottomleft", legend=c("assumed good", coi[c2use]),
             pch=pch, col=c("black", legend.cols[c2use]),
             bty="n", cex=cex)
    }
  }
}
