

#' generate color mapping with regions of sharp separation
#' 
#' For example, it converts this input
#' `c('green'=0,'green4|red'=2,'red4'=4)` to
#' `c('green'=0,'green4'=2,'red'=2+10e-6,'red4'=4)`
#'
#' @param colMap a named numerical vector; colors at point of separation should
#'    be separated by '|'.
#'
#' @return a normal color mapping vector;
#' @export
#'
#' @examples 
#' colMapDistinctRegions(c('green'=0,'green4|red'=2,'red4'=4))
colMapDistinctRegions=function(colMap){
  newCM=colMap
  inds=grep('|',names(colMap),fixed = T)
  if(length(inds)>0){
    names(newCM)=sub('\\|.*','',names(newCM))
    k=colMap[inds]+10e-6
    names(k)=sub('.*\\|','',names(k))
    newCM=c(newCM,k)
    newCM=sort(newCM)
  }
  
  newCM
}


#' Set the color mapping of a numerical vector, and the tick and tick labels of its legend
#' 
#' The purpose of this function is to auto-handle outliers in the numerical vectors,
#' because outliers compress the color mapping and make it difficult to distinguish
#' among the values of non-outliers.Here are the ways to achieve that goal in this 
#' function if outliers are present:  
#'   1. only non-outliers are used in color-value mapping.
#'   2. a '<' or '>' sign is prefixed to the first or last tick labels.
#' 
#' @param vec numerical; a vector or matrix
#' @param colMap a named numerical vector for the color mapping,`NULL`; 
#'   if `colMap` is not provided, the names of `colMap` is set as the followings:
#'     *  if `isAnnotation`, `c('white',x)` where x is a random color from 
#'         `getDistinctColors`.
#'     *  otherwise, `c('green4', 'white', 'red')`.
#' @param whiteValue Numeric, `NA`; it is the value of the white or middle color
#'        in the legend if `colMap` consists of three colors representing, e.g.
#'        up, no change, down, etc.
#' @param intAsDiscreteCutoff integer, 6; if `mat1` is an integer matrix and has < 6
#'        unique values, the color mapping in the legend will be discrete.
#' @param rmLegendOutliers logical, TRUE; If `colMap` is not provided,
#'   if `FALSE`, the range of legend ticks covers all values of `vac` 
#'   if `TRUE`, the range of legend ticks covers only non-outliers
#' @param isAnnotation logical, `FALSE`; if `TRUE`, the legend is for the annotation
#'   of a heatmap, not for heatmap itself; 
#' @param legendTicks numerical vector, NULL; 
#' @param legendTickLabels character vector, NULL; 
#'
#' @return a list of following items
#'   1. `colmap`: the completed `colMap`
#'   2. `legendParas`: `list(at = legendTicks,labels = legendTickLabels)`. it may 
#'        also contains `legend_gp` if `vec` is discrete integers; passed to
#'        `ComplexHeatmap::HeatmapAnnotation(annotation_legend_param)`
#'   3. `newVals`: infinite values in `vec`, if present, are set to `NA`
#'   4. `isDiscrete`: is `vec` discrete?
#' @export
#'
#' @examples # none
setColMapAndLegend=function(vec,
    colMap=NULL,
    whiteValue = NA,
    intAsDiscreteCutoff=6,
    rmLegendOutliers=T, 
    isAnnotation=F,
    legendTicks=NULL, legendTickLabels=NULL){
  
  re=list(colmap=NULL,legendParas=list(),newVals=NULL, isDiscrete=F)
  
  
  # char or logical vectors --------
  if ( is.character(vec) ||  is.logical(vec)) {
    re$isDiscrete=T
    # if color mapping is provided, check they contains all the values
    uniItems <- sort(unique(as.vector(vec)))
    if(!is.null(colMap) && !is.na(colMap[1])){
      stopifnot(all(setdiff(uniItems,NA) %in% colMap))
      re$colmap <- vecSwitch(colMap)
    }
    
    # numerical vectors ----------
  }else if(is.numeric(vec)){
    
    inds=which(!is.finite(vec))
    if(length(inds)>0){
      vec[inds]=NA
      re$newVals=vec
    }
    
    
    if(is.null(colMap)){
      if(isAnnotation) colMap=setNames(c(NA,NA),c('white',getDistinctColors(1)))
      else colMap=c('green4'=NA, 'white'=whiteValue, 'red'=NA)
    }else if(is.null(names(colMap))) {
      if (length(colMap) != 3)
        stop('if colMap is supplied with numbers only, it must be length 3')
      names(colMap) <- c('green4', 'white', 'red')
    }
    
    min1 <- min(as.vector(vec), na.rm = T)
    max1 <- max(as.vector(vec), na.rm = T)
    cmLen=length(colMap)
    
    if (NA %in% colMap[c(1, cmLen)]) {
      if (rmLegendOutliers) {
        olInds <- unlist(getOutliersInd(vec)) #indices of outliers
        nolInds <- setdiff(1:length(as.vector(vec)), olInds) #indices of non-outliers
        min2=min(vec[nolInds], na.rm = T)
        max2=max(vec[nolInds], na.rm = T)
        
        madj=0.1
        if (is.na(colMap[1]))
          colMap[1] <- ifelse(min1==min2, min1,ifelse(min2>0,min2*(1-madj),min2*(1+madj))) #min2 #
        if (is.na(colMap[cmLen]))
          colMap[cmLen] <- ifelse(max1==max2, max1,ifelse(max2>0,max2*(1+madj),max2*(1-madj))) #max2 #
        
        # if only one unique value remaining after removing outliers, the nearest
        #  outliers is used as one of the two ends of colMap
        if(colMap[1]==colMap[cmLen]){
          warning('After rmLegendOutliers, only one unique value remains. the nearest
            outliers is used as one of the two ends of colMap')
          
          mat2 <- abs(vec) - abs(colMap[1])
          inds0 <- which(mat2==min(mat2[mat2!=0],na.rm = T))
          if(vec[inds0][1]<colMap[1]) {
            colMap[1] <- vec[inds0][1]
          }else{
            colMap[cmLen] <- vec[inds0][1]
          }
        }
      } else{
        if (is.na(colMap[1]))
          colMap[1] <- min1
        if (is.na(colMap[cmLen]))
          colMap[cmLen] <- max1
      }
    } 
    
    if (length(colMap)==3) {
      if(!is.na(whiteValue)) colMap[2]=whiteValue
      if (is.na(colMap[2]) ||
          colMap[2] < colMap[1] || colMap[2] > colMap[cmLen]) {
        colMap[2] <- mean(colMap[c(1, cmLen)])
      }
    }
    
    re$colmap <- colMapDistinctRegions(colMap)
    
    
    
    vec1=as.vector(vec[!is.na(vec)])
    uniVals=sort(unique(vec1))
    isInt=all(vec1%%1==0)
    
    # discrete integers-------
    if(isInt && length(uniVals) < intAsDiscreteCutoff){
      re$isDiscrete=T
      #uniVals=uniVals[uniVals>=colMap[1] & uniVals<=colMap[cmLen]]
      #re$colmap=circlize::colorRamp2(unname(colMap), names(colMap))
      col_fun = circlize::colorRamp2(unname(colMap), names(colMap))
      re$colmap=setNames(col_fun(uniVals),uniVals)
      
      if(is.null(legendTicks)) { legendTicks = uniVals }
    }else{ #non-discrete
      # set legend ticks and labels; colMap only defines the mapping function,
      #   does not set legend ticks and labels ----
      if(is.null(legendTicks)) {
        legendTicks = unname(colMap)
        k=NULL
        for(i in 2:length(legendTicks))
          if((legendTicks[i]-legendTicks[i-1])<10e-5) k=c(k,i)
        
        if(!is.null(k)) legendTicks=legendTicks[-k]
        
        
        # each legend have five ticks
        if(length(legendTicks)==2){
          legendTicks=c(legendTicks[1],
                        mean(legendTicks[1:2]),
                        legendTicks[2])
        }
        
        if(length(legendTicks)==3){
          legendTicks=c(legendTicks[1],
                        mean(legendTicks[1:2]),
                        legendTicks[2],
                        mean(legendTicks[2:3]),
                        legendTicks[3])
        }
        
        if(isInt) legendTicks=as.integer(legendTicks)
      }
    }
      
    if(is.null(legendTickLabels)) {
      legendTickLabels <- `if`(re$isDiscrete,legendTicks, formatNum(legendTicks,less1 = T))
      if (legendTicks[1] > min1)
        legendTickLabels[1] <- paste0('<', legendTickLabels[1])
      if (legendTicks[length(legendTicks)] < max1)
        legendTickLabels[length(legendTickLabels)] <- paste0('>', legendTickLabels[length(legendTickLabels)])
    }
    
  }
  
  if(!is.null(legendTicks)) re$legendParas$at=legendTicks
  if(!is.null(legendTickLabels)) re$legendParas$labels=legendTickLabels
  re

}





#' Set the color mapping, tick, and tick labels of annotation legends.
#' 
#' Specifically, it does the following:
#'   1. for non-numerical variables in `annoDf`,create the color mapping using
#'      colors in `getDistinctColors()` because their default colors are not 
#'      very distinct.
#'   2. for numerical variables, apply `setColMapAndLegend()`
#'
#' @param annoDf data.frame; annotation
#' @param on 1:2; for row or column annotation
#' @param annoColMapList list of color mappings for each annotation, `NULL`; its
#'    value is passed to `col` in `ComplexHeatmap::HeatmapAnnotation()`.
#'    `names(annoColMapList)` are among `colnames(annoDf)`
#' @param annoParaList list of annotation parameters except `col` in 
#'   `ComplexHeatmap::HeatmapAnnotation()`, `list()`; 
#' @param rmLegendOutliers logical, `TRUE`; passed to [setColMapAndLegend()]
#' @param intAsDiscreteCutoff integer, 6; passed to [setColMapAndLegend()]

#'
#' @return a list() of the following items:
#'   1. `ha`: an object of `ComplexHeatmap::HeatmapAnnotation()` where the col and 
#'    legend tick and labels are reset based on `rmLegendOutliers`
#'   2. `newDf`: infinite values in numerical columns of `annoDf`, if present, are set to `NA`
#'    
#' @export
#'
#' @examples # none
setAnnoLegend=function(mat1,annoDf,annoColMapList=NULL,
                       annoParaList=list(),
                       on=1:2, rmLegendOutliers=T,
                       intAsDiscreteCutoff = 6){
  
  nFun=list(nrow,ncol)[[on]]
  nameFun=list(rownames,colnames)[[on]]
  hasInf=F
  
  #columns must be at the same order as the columns of mat1
  stopifnot(nrow(annoDf) == nFun(mat1))
  if (!is.null(rownames(annoDf))) {
    stopifnot(all(rownames(annoDf) == nameFun(mat1)))
  }
  
  cmList <- list()
  annoLgdParaList= `if`('annotation_legend_param' %in% names(annoParaList),
                        annoParaList[['annotation_legend_param']],list())
  for (i in colnames(annoDf)) {
    re=setColMapAndLegend(annoDf[, i],colMap=annoColMapList[[i]],
                          intAsDiscreteCutoff = intAsDiscreteCutoff,
                          rmLegendOutliers=rmLegendOutliers, 
                          isAnnotation=T,
                          legendTicks = annoLgdParaList[[i]][['at']],
                          legendTickLabels = annoLgdParaList[[i]][['labels']])
    
    if(!is.null(re$newVals)) {
      annoDf[, i]=re$newVals
      hasInf=T
    }
    
    if(length(re$legendParas)>0){
      if(i %in% names(annoLgdParaList))
        annoLgdParaList[[i]][names(re$legendParas)]=re$legendParas
      else annoLgdParaList[[i]]=re$legendParas
    }
    
    cmList[[i]] <-`if`(re$isDiscrete,re$colmap,circlize::colorRamp2(unname(re$colmap), names(re$colmap)))
  }
  
  
  if(length(annoLgdParaList)>0) annoParaList$annotation_legend_param=annoLgdParaList
  tmpList <- c(list(df = annoDf, col = cmList, which=c("row","column")[on]),annoParaList)
  tmpList=cleanList(tmpList)
  ha=do.call(ComplexHeatmap::HeatmapAnnotation, tmpList)
  newDf=`if`(hasInf,annoDf,NULL)
  list(ha=ha,newDf=newDf)
}


#' Remove items of zero length from a list
#'
#' @param x a list;
#'
#' @return a list
#' @export
#'
#' @examples #none
cleanList=function(x){
  inds=NULL
  for (i in 1:length(x)) {
    if(length(x[[i]])>0) inds=c(inds,i)
  }
  x[inds]
}




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
    # orange, "black",
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





#' format a numeric vector based on its distribution
#' 
#' @param v a numeric vector
#' @param less1 logical, FALSE; should the number of digits after formatting 
#'   be one less than the normal number of digits; 
#' @param rmLeading0 logical, TRUE; removing leading 0 for dicimals.
#'
#' @return a character vector
#' @export
#'
#' @examples 
#' formatNum(1234.5)
formatNum=function(v,less1=F,rmLeading0=F){
  
  setNsmall=function(v,nSmall){
    if(max(abs(v),na.rm = TRUE)>999){
      k=format(round(v,digits = nSmall),big.mark = ',',nsmall = nSmall)
    }else{
      k=format(round(v,digits = nSmall),nsmall = nSmall)
    }
  }
  
  k=v
  inds=which(is.finite(v) & v!=0)
  if(length(inds)>0){
    v2=abs(v[inds])
    vMin = min(v2,na.rm = TRUE)
    vMed = median(v2,na.rm = TRUE)
    vMax = max(v2,na.rm = TRUE)
    
    if(vMed>=1000000 || vMed < 0.01){
      k=format(v,scientific = TRUE,digits = 2-less1)
    }else if(vMed>=100 || all(v2%%1==0)){
      # 2nd case is integers in (-999,999) but has decimal
      #   points in representation, i.e. 30.00
      k=setNsmall(v,0)
    }else if(vMed>=10){
      k=setNsmall(v,1-less1)
    }else if(vMed>=1){
      k=setNsmall(v,2-less1)
    }else if(vMin>=0.1){
      k=setNsmall(v,2+(max(v,na.rm = TRUE)<=1)-less1)
    }else if(vMin>=0.01){
      k=setNsmall(v,3-less1)
    }else{
      k=format(v,scientific = TRUE,digits = 2-less1)
    }
  }
  
  k
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
  setNames(names(vec1),unname(vec1))
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