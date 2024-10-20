#' determine if a vector, matrix, or data.frame contains NA, NaN, Inf, -Inf
#'
#' @param x a vector, matrix, or data.frame
#'
#' @return a logical value
#' @export
#'
#' @examples
#' k=c(1:3,Inf)
#' hasInfinite(k)
#' k=factor(k)
#' !is.finite(k)
#' hasInfinite(k)
hasInfinite=function(x){
  hasInfiniteInVector=\(y) any(as.character(y) %in% as.character(c(NA,NaN, Inf, -Inf)))

  if(is.null(x)){ F
  }else if(is.data.frame(x)){
    k=sapply(x, hasInfiniteInVector)
    any(k)
  }else if(is.vector(x) || is.matrix(x) || is.factor(x)){
    hasInfiniteInVector(x)
  }else {
    stop('the input to hasInfinite() is not data.frame or vector!')
  }
}



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



#' add a distinct color for a specific value in the legend color map
#'
#' @param ColMap a named numeric vector,e.g. `c('yellow'=0,'red'=8.99,'green4*'=9)`
#' @param s1,s2 portion of the whole legend; they are used to control the width
#'   of the color band for the specific value; for example
#'   - `c('yellow'=0,'red'=8.95,'green4*'=9)`, the width is `8.95*(1+s2) to 9`
#'   - `c('yellow*'=0,'red'=0.05,'green4'=9)`, the width is `0 to 0.05*(1-s2)`
#'   - `c('yellow'=0,'red*'=1,'green4'=9)`, the width is `1*(1-s1) to 1*(1+s1)`
#' @return a list of two numeric vectors
#'   - colmap: the adjusted color maps
#'   - breaks: the breaks for `colmap`
#'
#' @examples # examples at https://blueskypie.github.io/cHeatmap/articles/cHeatmap-intro.html
addSingleValue=function(ColMap,s1=0.05,s2=0.001){
  re1 <- ColMap
  inds <- grep('*',names(ColMap),fixed = T)
  breakAt <- unname(ColMap)

  if(length(inds)>0){
    names(ColMap) <- sub('*','',names(ColMap),fixed = T)

    cmList=list()
    cml=length(ColMap)
    for (i in 1:length(inds)) {
      ind=inds[i]
      ColMap3=ColMap
      ColMap2 <- c(ColMap[ind],ColMap[ind])

      if(ind==1){
        ColMap2[2] <- ColMap[2]*(ifelse(ColMap[2]>0, 1-s2, 1+s2))
        ColMap3 <- c(ColMap2,ColMap[(ind+1):cml])

        breakAt <- breakAt[-2]
      }else if(ind==cml){
        ColMap2[1] <- ColMap[ind-1]*(ifelse(ColMap[ind-1]>0, 1+s2, 1-s2))
        ColMap3 <- c(ColMap[1:(ind-1)],ColMap2)

        breakAt <- breakAt[-(cml-1)]
      }else{
        cf <- circlize::colorRamp2(ColMap[c(ind-1,ind+1)],names(ColMap[c(ind-1,ind+1)]))
        ColMap2 <- c(ColMap[ind-1],ColMap2,ColMap[ind+1])
        ColMap2[1] <- unname(ColMap[ind])*(1-s1-s2)
        ColMap2[2] <- ColMap[ind]*(1-s1)
        ColMap2[3] <- ColMap[ind]*(1+s1)
        ColMap2[4] <- unname(ColMap[ind])*(1+s1+s2)
        names(ColMap2)[c(1,4)] <- cf(unname(ColMap[ind]))
        ColMap3 <- c(ColMap[1:(ind-1)],ColMap2,ColMap[(ind+1):cml])
      }

      cmList[[i]]=ColMap3
    }

    re1 <- sort(unlist(cmList))
  }

  list(colmap=re1,breaks=breakAt)
}
