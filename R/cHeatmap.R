#' Plot heatmaps
#'
#' This is a wrapper of the [ComplexHeatmap::Heatmap()] with additional
#' functions and more friendly interfaces for some common tasks in plotting heatmaps.
#'
#' Here are the features:

#' 1. Automatic or manual exclusion of outliers in the color mapping so
#' that the color scale of the heatmap and annotations is not dominated by outliers.
#' 2. The option to set the color-value mappings for the body and annotations of heatmaps.
#' 3. Automatic coloring of the dendrogram.
#' 4. Easy highlight or display of the values of certain cells in the body and 
#'    annotations of the heatmap.
#' 5. Discrete color-value mapping for integer matrices containing few unique values.
#' 6. Clustering of the character matrix based on the orders of characters.
#' 7. Interface to plot across rows.
#' 8. Handling of edge cases:
#'    * `Inf` and `-Inf` values in input matrix cause errors in `stats::dist()` for
#'      clustering, they are reset as NA.
#'    * If missing values are present in the input matrix or row and column
#'      annotations, a legend for missing value is added.
#'    * Plot a heatmap of a data.frame with columns of different data types 
#'      clustered by one of the columns.
#'     
#'
#' @param mat1 A numeric or character matrix or data frame, required.
#' @param NA.color character,'grey'; the color for missing values in `mat1`,
#'        `rowAnnoDf`, or `colmAnnoDf`. If missing values are present, a square
#'        of `NA.color` will be added to the legend.
#' @param whiteValue Numeric, `NULL`; it is the value of the white or middle color
#'        in the legend if `mat1` is numeric and `colMap` consists of three colors.
#' @param colMap A vector, `c("green4"=NA, "white"=whiteValue, "red"=NA)`;
#'        it defines the colors used in the heatmap and their mapping to values,
#'        and therefore should be supplied as a named vector.
#'    * For character `mat1`, `colMap` is a character vector, i.e.
#'        `c('red'='a','pink'='b','yellow'='c')` if the unique
#'        values in `mat1` are `a`, `b`, and `c`.
#'    * For numeric `mat1`, `colMap` is a numeric vector, i.e.
#'         `c('blue'= NA,'green'=5, 'red'=10)`
#'      1. `colMap` can be supplied w/o names for convenience,
#'        i.e. `c(-1, 0, 3)`; in this case, the length must be three and names are
#'        assumed to be `c("green4", "white", "red")`
#'      2. The first and last value of `colMap` are the lower and upper bounds of
#'         displayed values; values outside of that range will be considered as
#'         outliers.
#'      3. One or both of the first and last values of `colMap` can be supplied as NA;
#'         then outliers will be auto-computed if `rmLegendOutliers` is `TRUE`.
#' 
#' @param intAsDiscreteCutoff integer, 6; if `mat1` is an integer matrix and has < 6
#'        unique values, the color mapping in the legend will be discrete.
#' @param legendPos one of `c("right", "left", "bottom", "top")`
#' @param nRowCluster,nColmCluster integer, `NULL`; number of colored clusters in row or
#'        column; different clusters will be in different colors. If this argument
#'        is set, it assumes `cluster_rows` and `cluster_columns` are `TRUE`; so
#'        do __NOT__ explicitly set those two arguments again.
#'         To also split, i.e. columns, set `column_split = nColmCluster`.
#' @param rowAnnoDf,colmAnnoDf matrix or data frame, `NULL`; annotation for row
#'        or column. The rows of the `rowAnnoDf`/`colmAnnoDf` should be of the same
#'        length and order of the row/column of `mat1`.
#' @param rowAnnoPara,colmAnnoPara list, `list(na_col = NA.color)`; passed to
#'        [ComplexHeatmap::rowAnnotation()] or [ComplexHeatmap::columnAnnotation()]
#' @param rowAnnoColMap,colmAnnoColMap list, `NULL`; the color mappings for row and column
#'        annotations are handled automatically. Use this parameter to manually
#'        set the color mappings, `names(list)` is the names of each annotation.
#'        Each item of the list is a vector named by colors, similar to parameter `colMap`.
#'    * If an annotation is numeric, the vector is numeric,
#'        i.e. `c("green4"= -1, "yellow"=0, "red"=5)`
#'    * If an annotation is not numeric, the vector is character,
#'        i.e. `c('red'='a','pink'='b','yellow'='c')`
#' @param drawHeatmap logical, `TRUE`; it uses [ComplexHeatmap::draw()] to merge all
#'        legends into one column and draw the final heatmap.
#'    * It should be set to `TRUE` to obtain the clustering orders, i.e. by `column_order()`
#'    * However, it should be `FALSE` if the returned
#'        heatmap from `cHeatmap()` will be concatenated with other heatmaps.
#'
#' @param rmLegendOutliers logical, `is.numeric(mat1)`; if TRUE, only non-outliers
#'    are used in the color-value mappings; passed to [setColMapAndLegend()]
#' @param cellFontSize,cellFontColor numeric, `9`; character, `"white"`; format
#'        text displayed in heatmap cells.
#'
#' @param cellFun `NULL`; it is used to display info for individual cells; text
#'        info can be formatted by `cellFontSize` and `cellFontColor`. here
#'        are different usages:
#'    * A function to determine where and what to display at certain cells; it
#'      takes a cell value as input and the returned value is to be displayed.
#'      for example:
#'      + `cellFun = function(x){x}`: display the value of each cell
#'      + `cellFun = function(x){if(x > 10) x}`: display the value of cells
#'        whose values are greater than 10
#'      + `cellFun = function(x){if(x > 10) '?'}`: display the '?' sign
#'      + `cellFun = function(x){if(x > 10) list('rect',col='black',lwd=2)}`:
#'         color the edges black and set line width as 2. Check the parameters
#'         of [grid::gpar()] for more controls.
#'    * A character vector or list:
#'      +  `cellFun = 'o'`: display the values of all the outliers in `mat1` if any
#'      +  `cellFun = c('o','+')`: display the '+' sign instead of the value at the
#'         outlier cells
#'      +  `cellFun=c('o',list('rect',col='red',lwd=2))`: color the outlier cells
#'         with red edges
#'
#'    The `'o'` and `'rect'` are hard coded cases for usage convenience; for
#'       more complicated cell drawing, use the parameter `cell_fun` of
#'       [ComplexHeatmap::Heatmap()] directly.
#'       
#' @param annoCellFunList list of functions to define the indices and content to display values 
#'   of annotation, `NULL`; to use this, `drawHeatmap` must be `TRUE`;  
#'     the returned value of each function can be the followings:
#'    * A vector, the actual values of the annotation are displayed
#'      + if the vector is 'o', the cells to display are the outliers
#'      + otherwise, the vector should be indices in the original `rowAnnoDf` or
#'       `colmAnnoDf`
#'    * A list of two vectors
#'      + the 1st vector is defined as above
#'      + the 2nd vector is a single string as the content to display
#'  for example, here the columns `height` and `name` are used as annotation: 
#'    * `annoCellFunList = list(height=\(x){ which(x>6)},name=\(x) {1:2}))`: 
#'       display the heights greater than 6 and the first two names
#'    * `annoCellFunList = list(height=\(x){'o'},name=\(x) {list(1:2,'x')}))`: 
#'       display the outlier heights and the first two names as 'x'

#' @param cfMat `mat1`; the matrix for the *cells* in `cellFun`; it should have
#'    the same dimension as `mat1`. It gives the option of using a different
#'    matrix for the evaluation of `cellFun` and display its results on top of
#'    the heatmap from `mat1`.

#' @param rowDraw list, `NULL`; it is used to plot across rows. The list should
#'        contain the following item:
#'    1. the function in the `grid` package to make the draw and its parameters;
#'       for examples:
#'       - `list('grid.points',size = grid::unit(0.3, "char"),gp=grid::gpar(col='black'))`
#'         ; see [grid::grid.points()] for details. For `grid.point`, it can also
#'         be simply coded as `list('grid.points',size = 0.3,pch=1,col='black')`;
#'         parameter `col` and followed items are for `gpar()`.
#'       - `list('grid.lines',gp=grid::gpar(col='black',lwd = 2))`; see
#'         [grid::grid.lines()] for details. For `grid.lines`, it can also be
#'         simply coded as `list('grid.lines',col='black',lwd = 2)`
#'         parameter `col` and followed items are for `gpar()`.
#'       - `list(
#'           list('grid.points',size = 0.3,col='black'),
#'           list('grid.lines',col='black',lwd = 2)
#'           )` for multiple drawings
#'    2. a matrix containing the data to make the drawing, its number of columns
#'    3. an integer vector specifying the rows to make the drawing;
#'       if `NULL`, it means all the rows of mat1 and assumes
#'       `nrow() == nrow(mat1)` for the supplied matrix at #2
#'
#'    See the parameter `layer_fun` of [ComplexHeatmap::Heatmap()] for more
#'       complex drawing across a block of cells.
#' @param legendBreakDist numeric vector, NULL; set the distance portion between two breaks in
#'        the legend and can be the following values:
#'    * `NULL`; the numeric distance among breaks
#'    * `1`; equal distance among breaks
#'    * a numeric vector to represent the relative length of each section between
#'       two breaks; its length should be `length(legendTicks) - 1`;
#' @param legendHeight numeric, NULL; height of the vertical legend in cm.
#' @param legendTicks numeric, NULL; the legend tick; default is the `unname(colMap)`
#'    but can be any numeric vector whose range is within `unname(colMap)`.
#' @param legendTickLabels character, NULL; the label of legend tick; should be the
#'   same length as the `legendTicks`; use '' to skip a tick.
#' @param asGGplot logical, FALSE; to output as ggplot object, `drawHeatmap` must be `TRUE`

#' @param ... passed to [ComplexHeatmap::Heatmap()]
#'
#' @return a ggplot object if `asGGplot = TRUE`, or
#'         a [ComplexHeatmap::HeatmapList-class] object if `drawHeatmap = TRUE`, or
#'         a [ComplexHeatmap::Heatmap-class] object otherwise.
#' @export
#' @examples # examples at https://blueskypie.github.io/cHeatmap/articles/cHeatmap-intro.html
cHeatmap <- function(mat1,
                     NA.color='grey',
                     whiteValue = NA,
                     colMap = c("green4" = NA,
                                "white" = whiteValue,
                                "red" = NA),
                     intAsDiscreteCutoff = 6,
                     legendPos = c("right", "left", "bottom", "top"),
                     nRowCluster = NULL,
                     nColmCluster = NULL,
                     rowAnnoDf = NULL,
                     colmAnnoDf = NULL,
                     rowAnnoPara = list(na_col = NA.color),
                     colmAnnoPara = list(na_col = NA.color),
                     rowAnnoColMap = NULL,
                     colmAnnoColMap = NULL,
                     drawHeatmap = T,
                     rmLegendOutliers = is.numeric(mat1),
                     cellFun = NULL,
                     annoCellFunList=NULL,
                     cfMat = mat1,
                     cellFontSize = 9,
                     cellFontColor = 'black',
                     rowDraw = NULL,
                     legendBreakDist = NULL,
                     legendHeight = NULL,
                     legendTicks = NULL,
                     legendTickLabels = NULL,
                     asGGplot = FALSE,
                     ...) {
  
  asgg=asGGplot
  thiscall <- match.call(expand.dots = TRUE)
  thiscall=as.list(thiscall)
  thiscall['asGGplot']=NULL
  
  if(asgg){
    ggplotify::as.ggplot(function(){do.call(cHeatmap0,thiscall[-1])})
  }else{
    do.call(cHeatmap0,thiscall[-1])
  }
}



#' Plot heatmaps
#'
#' This is a wrapper of the [ComplexHeatmap::Heatmap()] with additional
#' functions and more friendly interfaces for some common tasks in plotting heatmaps.
#'
#' Here are the features:

#' 1. Automatic or manual exclusion of outliers in the color mapping so
#' that the color scale of the heatmap and annotations is not dominated by outliers.
#' 2. The option to set the color-value mappings for the body and annotations of heatmaps.
#' 3. Automatic coloring of the dendrogram.
#' 4. Easy highlight or display of the values of certain cells in the body and 
#'    annotations of the heatmap.
#' 5. Discrete color-value mapping for integer matrices containing few unique values.
#' 6. Clustering of the character matrix based on the orders of characters.
#' 7. Interface to plot across rows.
#' 8. Handling of edge cases:
#'    * `Inf` and `-Inf` values in input matrix cause errors in `stats::dist()` for
#'      clustering, they are reset as NA.
#'    * If missing values are present in the input matrix or row and column
#'      annotations, a legend for missing value is added.
#'    * Plot a heatmap of a data.frame with columns of different data types 
#'      clustered by one of the columns.
#'     
#'
#' @param mat1 A numeric or character matrix or data frame, required.
#' @param NA.color character,'grey'; the color for missing values in `mat1`,
#'        `rowAnnoDf`, or `colmAnnoDf`. If missing values are present, a square
#'        of `NA.color` will be added to the legend.
#' @param whiteValue Numeric, `NULL`; it is the value of the white or middle color
#'        in the legend if `mat1` is numeric and `colMap` consists of three colors.
#' @param colMap A vector, `c("green4"=NA, "white"=whiteValue, "red"=NA)`;
#'        it defines the colors used in the heatmap and their mapping to values,
#'        and therefore should be supplied as a named vector.
#'    * For character `mat1`, `colMap` is a character vector, i.e.
#'        `c('red'='a','pink'='b','yellow'='c')` if the unique
#'        values in `mat1` are `a`, `b`, and `c`.
#'    * For numeric `mat1`, `colMap` is a numeric vector, i.e.
#'         `c('blue'= NA,'green'=5, 'red'=10)`
#'      1. `colMap` can be supplied w/o names for convenience,
#'        i.e. `c(-1, 0, 3)`; in this case, the length must be three and names are
#'        assumed to be `c("green4", "white", "red")`
#'      2. The first and last value of `colMap` are the lower and upper bounds of
#'         displayed values; values outside of that range will be considered as
#'         outliers.
#'      3. One or both of the first and last values of `colMap` can be supplied as NA;
#'         then outliers will be auto-computed if `rmLegendOutliers` is `TRUE`.
#'
#' @param intAsDiscreteCutoff integer, 6; if `mat1` is an integer matrix and has < 6
#'        unique values, the color mapping in the legend will be discrete.
#' @param legendPos one of `c("right", "left", "bottom", "top")`
#' @param nRowCluster,nColmCluster integer, `NULL`; number of colored clusters in row or
#'        column; different clusters will be in different colors. If this argument
#'        is set, it assumes `cluster_rows` and `cluster_columns` are `TRUE`; so
#'        do __NOT__ explicitly set those two arguments again.
#'         To also split, i.e. columns, set `column_split = nColmCluster`.
#' @param rowAnnoDf,colmAnnoDf matrix or data frame, `NULL`; annotation for row
#'        or column. The rows of the `rowAnnoDf`/`colmAnnoDf` should be of the same
#'        length and order of the row/column of `mat1`.
#' @param rowAnnoPara,colmAnnoPara list, `list(na_col = NA.color)`; passed to
#'        [ComplexHeatmap::rowAnnotation()] or [ComplexHeatmap::columnAnnotation()]
#' @param rowAnnoColMap,colmAnnoColMap list, `NULL`; the color mappings for row and column
#'        annotations are handled automatically. Use this parameter to manually
#'        set the color mappings, `names(list)` is the names of each annotation.
#'        Each item of the list is a vector named by colors, similar to parameter `colMap`.
#'    * If an annotation is numeric, the vector is numeric,
#'        i.e. `c("green4"= -1, "yellow"=0, "red"=5)`
#'    * If an annotation is not numeric, the vector is character,
#'        i.e. `c('red'='a','pink'='b','yellow'='c')`
#' @param drawHeatmap logical, `TRUE`; it uses [ComplexHeatmap::draw()] to merge all
#'        legends into one column and draw the final heatmap.
#'    * It should be set to `TRUE` to obtain the clustering orders, i.e. by `column_order()`
#'    * However, it should be `FALSE` if the returned
#'        heatmap from `cHeatmap()` will be concatenated with other heatmaps.
#'
#' @param rmLegendOutliers logical, `is.numeric(mat1)`; if TRUE, only non-outliers
#'    are used in the color-value mappings; passed to [setColMapAndLegend()]
#' @param cellFontSize,cellFontColor numeric, `9`; character, `"white"`; format
#'        text displayed in heatmap cells.
#'
#' @param cellFun `NULL`; it is used to display info for individual cells; text
#'        info can be formatted by `cellFontSize` and `cellFontColor`. here
#'        are different usages:
#'    * A function to determine where and what to display at certain cells; it
#'      takes a cell value as input and the returned value is to be displayed.
#'      for example:
#'      + `cellFun = function(x){x}`: display the value of each cell
#'      + `cellFun = function(x){if(x > 10) x}`: display the value of cells
#'        whose values are greater than 10
#'      + `cellFun = function(x){if(x > 10) '?'}`: display the '?' sign
#'      + `cellFun = function(x){if(x > 10) list('rect',col='black',lwd=2)}`:
#'         color the edges black and set line width as 2. Check the parameters
#'         of [grid::gpar()] for more controls.
#'    * A character vector or list:
#'      +  `cellFun = 'o'`: display the values of all the outliers in `mat1` if any
#'      +  `cellFun = c('o','+')`: display the '+' sign instead of the value at the
#'         outlier cells
#'      +  `cellFun=c('o',list('rect',col='red',lwd=2))`: color the outlier cells
#'         with red edges
#'
#'    The `'o'` and `'rect'` are hard coded cases for usage convenience; for
#'       more complicated cell drawing, use the parameter `cell_fun` of
#'       [ComplexHeatmap::Heatmap()] directly.
#'       
#' @param annoCellFunList list of functions to define the indices and content to display values 
#'   of annotation, `NULL`; to use this, `drawHeatmap` must be `TRUE`;  
#'     the returned value of each function can be the followings:
#'    * A vector, the actual values of the annotation are displayed
#'      + if the vector is 'o', the cells to display are the outliers
#'      + otherwise, the vector should be indices in the original `rowAnnoDf` or
#'       `colmAnnoDf`
#'    * A list of two vectors
#'      + the 1st vector is defined as above
#'      + the 2nd vector is a single string as the content to display
#'  for example, here the columns `height` and `name` are used as annotation: 
#'    * `annoCellFunList = list(height=\(x){ which(x>6)},name=\(x) {1:2}))`: 
#'       display the heights greater than 6 and the first two names
#'    * `annoCellFunList = list(height=\(x){'o'},name=\(x) {list(1:2,'x')}))`: 
#'       display the outlier heights and the first two names as 'x'

#' @param cfMat `mat1`; the matrix for the *cells* in `cellFun`; it should have
#'    the same dimension as `mat1`. It gives the option of using a different
#'    matrix for the evaluation of `cellFun` and display its results on top of
#'    the heatmap from `mat1`.

#' @param rowDraw list, `NULL`; it is used to plot across rows. The list should
#'        contain the following item:
#'    1. the function in the `grid` package to make the draw and its parameters;
#'       for examples:
#'       - `list('grid.points',size = grid::unit(0.3, "char"),gp=grid::gpar(col='black'))`
#'         ; see [grid::grid.points()] for details. For `grid.point`, it can also
#'         be simply coded as `list('grid.points',size = 0.3,pch=1,col='black')`;
#'         parameter `col` and followed items are for `gpar()`.
#'       - `list('grid.lines',gp=grid::gpar(col='black',lwd = 2))`; see
#'         [grid::grid.lines()] for details. For `grid.lines`, it can also be
#'         simply coded as `list('grid.lines',col='black',lwd = 2)`
#'         parameter `col` and followed items are for `gpar()`.
#'       - `list(
#'           list('grid.points',size = 0.3,col='black'),
#'           list('grid.lines',col='black',lwd = 2)
#'           )` for multiple drawings
#'    2. a matrix containing the data to make the drawing, its number of columns
#'    3. an integer vector specifying the rows to make the drawing;
#'       if `NULL`, it means all the rows of mat1 and assumes
#'       `nrow() == nrow(mat1)` for the supplied matrix at #2
#'
#'    See the parameter `layer_fun` of [ComplexHeatmap::Heatmap()] for more
#'       complex drawing across a block of cells.
#' @param legendBreakDist numeric vector, NULL; set the distance portion between two breaks in
#'        the legend and can be the following values:
#'    * `NULL`; the numeric distance among breaks
#'    * `1`; equal distance among breaks
#'    * a numeric vector to represent the relative length of each section between
#'       two breaks; its length should be `length(legendTicks) - 1`;
#' @param legendHeight numeric, NULL; height of the vertical legend in cm.
#' @param legendTicks numeric, NULL; the legend tick; default is the `unname(colMap)`
#'    but can be any numeric vector whose range is within `unname(colMap)`.
#' @param legendTickLabels character, NULL; the label of legend tick; should be the
#'   same length as the `legendTicks`; use '' to skip a tick.

#' @param ... passed to [ComplexHeatmap::Heatmap()]
#'
#' @return a [ComplexHeatmap::HeatmapList-class] object if drawHeatmap, or
#'         a [ComplexHeatmap::Heatmap-class] object otherwise.
#' @examples # examples at https://blueskypie.github.io/cHeatmap/articles/cHeatmap-intro.html
cHeatmap0 <- function(mat1,
                      NA.color='grey',
                      whiteValue = NA,
                      colMap = c("green4" = NA,
                                 "white" = whiteValue,
                                 "red" = NA),
                      intAsDiscreteCutoff = 6,
                      legendPos = c("right", "left", "bottom", "top"),
                      nRowCluster = NULL,
                      nColmCluster = NULL,
                      rowAnnoDf = NULL,
                      colmAnnoDf = NULL,
                      rowAnnoPara = list(na_col = NA.color),
                      colmAnnoPara = list(na_col = NA.color),
                      rowAnnoColMap = NULL,
                      colmAnnoColMap = NULL,
                      drawHeatmap = T,
                      rmLegendOutliers = is.numeric(mat1),
                      cellFun = NULL,
                      annoCellFunList=NULL,
                      cfMat = mat1,
                      cellFontSize = 9,
                      cellFontColor = 'black',
                      rowDraw = NULL,
                      legendBreakDist = NULL,
                      legendHeight = NULL,
                      legendTicks = NULL,
                      legendTickLabels = NULL,
                      ...) {
  
  argList <- list( #arguments for ComplexHeatmap::Heatmap()
    na_col=NA.color,
    show_parent_dend_line = F,
    # add white edge for each cell
    rect_gp = grid::gpar(col = "white", lwd = 0.1)
  )
  
  
  if (is.data.frame(mat1))  mat1 <- as.matrix(mat1)
  
  # add a legend for missing values in mat1 or annotation
  addLgd4NA=hasInfinite(mat1) || hasInfinite(rowAnnoDf) || hasInfinite(colmAnnoDf)
  
  
  re=setColMapAndLegend(mat1,colMap=colMap,
                        intAsDiscreteCutoff=intAsDiscreteCutoff,
                        rmLegendOutliers=rmLegendOutliers,
                        isAnnotation=F,
                        legendTicks=legendTicks, legendTickLabels=legendTickLabels)
  
  
  
  if(!is.null(re$newVals)) mat1=re$newVals
  
  
  colMap=re$colmap
  cmLen <- length(colMap)
  argList$col <- `if`(re$isDiscrete,colMap,circlize::colorRamp2(unname(colMap), names(colMap)))
  
  
  # display cell values -----
  if (!is.null(cellFun)) {
    argList$cell_fun <- function(j, i, x, y, width, height, fill) {
      k <- NULL # the char to display
      # is outliers
      isOL=is.finite(cfMat[i, j]) && (cfMat[i, j] > colMap[cmLen] || cfMat[i, j] < colMap[1])
      
      if (is.character(cellFun)) {
        if (cellFun[1] == 'o' && isOL) {
          k <- ifelse(length(cellFun) > 1, cellFun[2], cfMat[i, j])
        }
      } else if (is.function(cellFun)) {
        k <- cellFun(cfMat[i, j])
      } else if (is.list(cellFun)) {
        if (cellFun[[1]] == 'o' && isOL) {
          k <- cellFun[-1]
        }
      }
      
      if (is.numeric(k) && is.finite(k)) {
        k <- formatNum(k)
      }
      
      if (is.character(k)) {
        grid::grid.text(k,
                        x,
                        y,
                        gp = grid::gpar(fontsize = cellFontSize, col = cellFontColor))
      }
      
      if (is.list(k)) {
        if (k[[1]] == 'rect') {
          if (is.null(k$fill))
            k$fill <- NA
          grid::grid.rect(
            x = x,
            y = y,
            width = width,
            height = height,
            gp = do.call(grid::gpar, k[-1])
          ) #list()
        }
      }
    }
  }
  
  
  # draw row figures ------
  if (!is.null(rowDraw)) {
    argList$layer_fun <- function(j, i, x, y, w, h, fill) {
      #browser()
      # layer_fun to plot line and point in each cell using data in mat1
      nRow <- length(unique(i)) #nRow in each slice
      #the start and end mark of each row in y-axis
      # i.e. for 5 rows, ry is 0.0 0.2 0.4 0.6 0.8 1.0
      ry <- seq(1, 0, -1 / nRow)
      
      rowIDs <- `if`(length(rowDraw) == 2, 1:nrow(mat1), rowDraw[[3]])
      for (rInd in 1:nRow) {
        # rInd: row index in the current slice
        # i[rInd]: row index in the original matrix
        # k: value of row i[rInd] of mat1\
        if (i[rInd] %in% rowIDs) {
          k <- which(rowIDs == i[rInd])
          Ys <- rowDraw[[2]][k, ]
          inds <- which(!is.na(Ys))
          
          if (length(inds) > 0) {
            Ys <- Ys[inds]
            Xs <- unique(x)[inds] #x coordinates of Ys in current slice
            Ys <- scales::rescale(Ys, c(ry[rInd + 1], ry[rInd]))
            
            #browser()
            for (f in rowDraw[1]) {
              if (is.list(f[[1]])) {
                # >1 grid.functions, i.e. list of list
                sapply(f, gridFuns, x = Xs, y = Ys)
              } else{
                gridFuns(Xs, Ys, f)
              }
            }
          }
        }
      }
    }
  }
  
  
  
  
  # row annotation -----
  if (!is.null(rowAnnoDf)) {
    re1=setAnnoLegend(mat1,rowAnnoDf,on=1, 
                      annoColMapList=rowAnnoColMap,
                      annoParaList=rowAnnoPara,
                      rmLegendOutliers=rmLegendOutliers,
                      intAsDiscreteCutoff = intAsDiscreteCutoff)
    argList$left_annotation=re1$ha
    if(!is.null(re1$newDf)) rowAnnoDf=re1$newDf
  }
  
  # column annotation -----
  if (!is.null(colmAnnoDf)) {
    re1=setAnnoLegend(mat1,colmAnnoDf,on=2, 
                      annoColMapList=colmAnnoColMap,
                      annoParaList=colmAnnoPara,
                      rmLegendOutliers=rmLegendOutliers,
                      intAsDiscreteCutoff = intAsDiscreteCutoff)
    argList$top_annotation=re1$ha
    if(!is.null(re1$newDf)) colmAnnoDf=re1$newDf
  }
  
  
  # collect heatmap_legend_param----
  lgdParas <- c(
    list(title_position = "topleft",break_dist = legendBreakDist),
    re$legendParas)
  
  if(!is.null(legendHeight)) {
    lgdParas$legend_height  <- grid::unit(legendHeight, 'cm')
  }
  
  dotArgs <- list(...)
  dotArgs$heatmap_legend_param <- c(dotArgs$heatmap_legend_param, lgdParas)
  
  # overwrite any hard-coded parameters with user supplied ones if any
  argList$matrix <- mat1
  argList[names(dotArgs)] <- dotArgs
  
  # clustering and color dendrogram ----
  if (!is.null(nRowCluster)) {
    row_dend <- stats::as.dendrogram(stats::hclust(stats::dist(mat1))) # row clustering
    argList$cluster_rows <- dendextend::color_branches(row_dend, k = nRowCluster)
  }
  if (!is.null(nColmCluster)) {
    colm_dend <- stats::as.dendrogram(stats::hclust(stats::dist(t(mat1)))) # row clustering
    argList$cluster_columns <- dendextend::color_branches(colm_dend, k = nColmCluster)
  }
  
  #set the height of heatmap body same as the height of each column annotation
  if(nrow(mat1)==1) argList$height=grid::unit(5, "mm")
  else if(ncol(mat1)==1) argList$width=grid::unit(5, "mm")
  
  ht1 <- do.call(ComplexHeatmap::Heatmap, argList)
  
  if (drawHeatmap) {
    lgd=list()
    if(addLgd4NA){
      lgd[[1]]=ComplexHeatmap::Legend(labels = 'NA',type = 'grid',
                                      legend_gp = grid::gpar(fill = NA.color,col=NA.color))
    }
    ht1=ComplexHeatmap::draw(ht1, heatmap_legend_list =lgd,
                             merge_legend = TRUE,
                             heatmap_legend_side = legendPos[1])
    
    # display certain values in the annotation -------
    if(!is.null(annoCellFunList)){ 
      f1=\(x,df){
        inds=NULL
        dStr=NULL
        re=annoCellFunList[[x]](df[[x]])
        if(re[1]=='o'){ # || re[[1]][1]=='o'
          inds=unlist(getOutliersInd(df[[x]]))
        }else if(all(re%%1==0)){ #check is all are integers
          inds=re
        }
        
        if(is.list(re)){
          if(is.null(inds)) inds=re[[1]]
          dStr=re[[2]]
        }
        
        if(is.null(dStr)) dStr=df[inds,x]
        
        if(is.numeric(dStr)) dStr=formatNum(dStr,less1=T)
        list(inds=inds,dStr=dStr)
      }
      
      
      for(aName in names(annoCellFunList)){
        if(aName %in% colnames(rowAnnoDf)){
          newOrders=ComplexHeatmap::row_order(ht1)
          mapInd=nrow(mat1)+1-order(newOrders)
          re=f1(aName,rowAnnoDf)
          if(length(re$inds)>0){
            hl=0.5/nrow(rowAnnoDf)
            ComplexHeatmap::decorate_annotation(aName, {
              grid::grid.text(re$dStr,y= hl*(2*mapInd[re$inds]-1),rot=90)
            })
          }
        }else{
          newOrders=ComplexHeatmap::column_order(ht1)
          mapInd=order(newOrders) #ncol(mat1)+1-order(newOrders) 
          re=f1(aName,colmAnnoDf)
          if(length(re$inds)>0){
            # i is the half width of a cell
            hl=0.5/nrow(colmAnnoDf)
            ComplexHeatmap::decorate_annotation(aName, {
              grid::grid.text(re$dStr,x=hl*(2*mapInd[re$inds]-1))
            })
          }
        }
      }
    }
  }else{  ht1 }
}
