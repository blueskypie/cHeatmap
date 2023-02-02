#' Get distinct colors
#'
#' @param n The number of distinct colors
#'
#' @return A character vector of distinct colors
#' @export
#'
#' @examples
#' getDistinctColors(5)
getDistinctColors <- function(n) {
  # allColors= c( #https://mokole.com/palette.html
  #   'darkgreen','darkblue','maroon3','orangered','yellow','burlywood','lime','aqua','fuchsia','cornflower'
  # )
  allColors <- c(
    "dodgerblue2",
    "#E31A1C",
    # red
    "green4",
    "#6A3D9A",
    # purple
    "#FF7F00",
    # orange
    "black",
    "gold4",
    "skyblue2",
    "#FB9A99",
    # lt pink
    "palegreen2",
    "#CAB2D6",
    # lt purple
    "#FDBF6F",
    # lt orange
    "gray70",
    "khaki2",
    "maroon",
    "orchid1",
    "deeppink1",
    "blue1",
    "steelblue4",
    "darkturquoise",
    "green1",
    "yellow4",
    "yellow3",
    "darkorange4",
    "brown"
  )

if (n > length(allColors)) {
  allColors <- grDevices::colors(distinct = T)
}
  #sample(allColors,n)
  allColors[1:n]
}



#' Get the number of rounding digits
#'
#' @param x A numeric number
#' @return
#' if abs(x) >= 10, returns 0
#' else if abs(x) >= 1, returns 1
#' else returns 2
#' @export
#'
#' @examples
#' nRoundDigits(2.38)
nRoundDigits <- function(x) {
  ifelse(abs(x) >= 10 || is.integer(x), 0, ifelse(abs(x) >= 1, 1, 2))
}



#' Get the indices of outliers in a numeric vector or matrix
#'
#' @description Outliers are < Q1-1.5IQR or > Q3+1.5IQR
#' @param x A numeric vector or matrix
#'
#' @return A list of two int vectors
#' 1. indices of outliers at the low end
#' 2. indices of outliers at the high end
#' @export
#'
#' @examples
#' getOutliersInd(c(1,100:110,1000))
getOutliersInd <- function(x) {
  qVals <- stats::quantile(x, na.rm = T)
  iqr = qVals[4] - qVals[2]
  lowerB <- qVals[2] - 1.5 * iqr
  upperB <- qVals[4] + 1.5 * iqr
  #inds=which(x<lowerB | x>upperB)
  return(list(low = which(x < lowerB), high = which(x > upperB)))
}


#' given a named vector, switch its content and name
#'
#' @param vec1 a named vector
#'
#' @return a named vector whose `names()` equals `vec1` and content is `names(vec1)`
#' @export
#'
#' @examples vecSwitch(c('red'='a','pink'='b','yellow'='c'))
vecSwitch <- function(vec1) {
  # k switches content and names with colMap
  k <- names(vec1)
  names(k) <- as.character(vec1)
  k
}


#' a wrapper for [grid::grid.points()] and [grid::grid.lines()]
#'
#' It's to process user-supplied functions. See the doc of the parameter `rowDraw`
#' of `cHeatmap` function for details
#'
#' @param x,y the x and y used in fun1
#' @param fun1 a list containg info of a grid function e.g.
#'             `list('grid.points',size = 0.3,pch=1,col='black')`
#'
#' @examples # none
gridFuns <- function(x, y, fun1) {
  if ('gp' %in% names(fun1)) {
    fun1[[1]] <- paste0('grid::', fun1[[1]])
    do.call(fun1[[1]], fun1[-1])
  } else if (grepl('grid.points', fun1[[1]])) {
    if (is.numeric(fun1$size)) {
      fun1$size <- grid::unit(fun1$size, 'char')
    }

    ind <- which(names(fun1) == 'col')
    stopifnot(length(ind) > 0) # col must be defined
    alist <- `if`(ind == 2, NULL, fun1[2:(ind - 1)])
    gp1 <- do.call(grid::gpar, fun1[ind:length(fun1)])
    do.call(grid::grid.points, c(list(
      x = x, y = y, gp = gp1
    ), alist))
  } else if (grepl('grid.lines', fun1[[1]]) && length(y) > 1) {
    ind <- which(names(fun1) == 'col')
    stopifnot(length(ind) > 0) # col must be defined
    alist <- `if`(ind == 2, NULL, fun1[2:(ind - 1)])
    gp1 <- do.call(grid::gpar, fun1[ind:length(fun1)])
    do.call(grid::grid.lines, c(list(
      x = x, y = y, gp = gp1
    ), alist))
  }
}
