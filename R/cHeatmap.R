#' Plot heatmaps
#'
#' This is a wrapper of the [ComplexHeatmap::Heatmap()] with additional
#' functions and more friendly interfaces for some common tasks in plotting heatmaps.
#'
#' Here are the features:

#' 1. Automatic or manual setting of the values of outliers in the input matrix so
#' that the color scale of the heatmap is not dominated by those outliers
#' 2. The option to set the color-value mappings in the heatmap legend
#' 3. Automatic coloring of the dendrogram
#' 4. Easy highlight or display of the values of certain cells
#' 5. Discrete color-value mapping for integer matrices containing few unique values
#' 6. Clustering of the character matrix based on the orders of characters
#' 7. Interface to plot across rows
#'
#' @param mat1 A numeric or character matrix or data frame, required.
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
#'         then outliers will be auto-computed if `resetOutliers` is `TRUE`.
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
#' @param rowAnnoPara,colmAnnoPara list, `list(na_col = "white")`; passed to
#'        [ComplexHeatmap::rowAnnotation()] or [ComplexHeatmap::columnAnnotation()]
#' @param rowAnnoColMap,colmAnnoColMap list, `list()`; the color mappings for row and column
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
#' @param resetOutliers logical, `is.numeric(mat1)`; outliers in numeric `mat1` can
#'        stretch the whole color scale. If `TRUE`, the bounds of the legend will
#'        be reset to the max or min values excluding them; for example,  the legend
#'        of `c(10, 0.1, 0, 0.2)` will be from `0` to `0.2` with the labels `0` and `>0.2`.
#' @param clusterUsingResetValues logical, `FALSE`; should the clustering of
#'        rows and columns use the reset values of outliers?
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
#'       two breaks; its length should be `length(colMap) - 1`;
#'       for example, `c(1, 1, 0.5, 3)` for
#'    `colMap = c("green4" = -1, "white" = 0, "red" = 1, "yellow" = 3, "blue" = 18)`
#' @param legendHeight numeric, NULL; height of the vertical legend in cm.

#' @param ... passed to [ComplexHeatmap::Heatmap()]
#'
#' @return a [ComplexHeatmap::HeatmapList-class] object if drawHeatmap, or
#'         a [ComplexHeatmap::Heatmap-class] object otherwise.
#' @export
#' @examples # examples at https://blueskypie.github.io/cHeatmap/articles/cHeatmap-intro.html
cHeatmap <- function(mat1,
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
                     rowAnnoPara = list(na_col = "white"),
                     colmAnnoPara = list(na_col = "white"),
                     rowAnnoColMap = list(),
                     colmAnnoColMap = list(),
                     drawHeatmap = T,
                     resetOutliers = is.numeric(mat1),
                     clusterUsingResetValues = FALSE,
                     cellFun = NULL,
                     cfMat = mat1,
                     cellFontSize = 9,
                     cellFontColor = 'black',
                     rowDraw = NULL,
                     legendBreakDist = NULL,
                     legendHeight = NULL,
                     ...) {
  if (is.data.frame(mat1))
    mat1 <- as.matrix(mat1)
  argList <- list(
    show_parent_dend_line = F,
    # add white edge for each cell
    rect_gp = grid::gpar(col = "white", lwd = 0.1)
  )

  mat0 <- mat1
  cmLen <- length(colMap)
  legendBounds <- NULL
  isDiscrete <- is.character(mat1) ||
    (is.integer(mat1) &&
       length(unique(as.vector(mat1))) < intAsDiscreteCutoff)


  if (isDiscrete) {
    resetOutliers <- F
    uniItems <- sort(unique(as.vector(mat1)))
    uniItems <- uniItems[!is.na(uniItems)]

    if (is.character(mat1) &&
        (!is.null(nRowCluster) || !is.null(nColmCluster))) {
      #Convert to integer for clustering
      m1 <- apply(mat1, 2, function(x) {
        as.integer(factor(x, levels = uniItems))
      })
      rownames(m1) <- rownames(mat1)
      mat1 <- m1
      rm(m1)
    }

    argList$heatmap_legend_param <- c(argList$heatmap_legend_param, list(labels =
                                                                           uniItems))

    # color mapping
    argList$col <- `if`(is.na(colMap[1]),
                        # user does not set colMap
                        # 1:length(uniItems) are indices of auto-generated colors
                        # names are the items in mat1 those colors map to
                        structure(1:length(uniItems), names = as.character(uniItems)),
                        vecSwitch(colMap))

  } else{
    # mat1 is numeric
    min1 <- min(as.vector(mat1), na.rm = T)
    max1 <- max(as.vector(mat1), na.rm = T)



    #fill in missing colors or numbers at the 1st or last indices in colMap -----
    if (is.null(names(colMap))) {
      if (cmLen != 3)
        stop('if colMap is supplied with numbers only, it must be length 3')
      names(colMap) <- c('green4', 'white', 'red')
    }

    if (NA %in% colMap[c(1, cmLen)]) {
      if (resetOutliers) {
        olInds <- unlist(getOutliersInd(mat1)) #indices of outliers
        nolInds <- setdiff(1:length(as.vector(mat1)), olInds) #indices of non-outliers

        if (is.na(colMap[1]))
          colMap[1] <- min(mat1[nolInds], na.rm = T)
        if (is.na(colMap[cmLen]))
          colMap[cmLen] <- max(mat1[nolInds], na.rm = T)

        if(colMap[1]==colMap[cmLen]){
          stop('After resetOutliers, only one unique value remains. Please set resetOutliers = F')
        }
      } else{
        if (is.na(colMap[1]))
          colMap[1] <- min1
        if (is.na(colMap[cmLen]))
          colMap[cmLen] <- max1
      }
    } else{
      resetOutliers <- colMap[1] > min1 || colMap[cmLen] < max1
    }

    if (length(colMap)==3) {
      if (is.na(colMap[2]) ||
          colMap[2] < colMap[1] || colMap[2] > colMap[cmLen]) {
        colMap[2] <- mean(colMap[c(1, cmLen)])
      }
    }


    # reset values if asked ----
    if (clusterUsingResetValues) {
      mat1[which(mat1 > colMap[cmLen])] <- colMap[cmLen]
      mat1[which(mat1 < colMap[1])] <- colMap[1]
    }


    # if only one unique value remaining after removing outliers, the nearest
    #  outliers is used as one of the two ends of colMap
    if(colMap[1]==colMap[cmLen]){
      mat2 <- abs(mat0) - abs(colMap[1])
      inds0 <- which(mat2==min(mat2[mat2!=0],na.rm = T))
      if(mat0[inds0][1]<colMap[1]) {
        colMap[1] <- mat0[inds0][1]
      }else{
        colMap[cmLen] <- mat0[inds0][1]
      }
    }

    argList$col <- circlize::colorRamp2(unname(colMap), names(colMap))
    #set legend to be used later and pass colMap info to a function
    legendBounds <- list(at = sapply(colMap, function(x) {
      round(x, nRoundDigits(x))
    }))
    #legendBounds$at <- unique(legendBounds$at)
    lbLabel <- as.character(legendBounds$at)
    if (colMap[1] > min1)
      lbLabel[1] <- paste0('<', lbLabel[1])
    if (colMap[cmLen] < max1)
      lbLabel[cmLen] <- paste0('>', lbLabel[cmLen])
    legendBounds$labels <- lbLabel
  }




  # display cell values -----
  if (!is.null(cellFun)) {
    argList$cell_fun <- function(j, i, x, y, width, height, fill) {
      k <- NULL # the char to display

      if (is.character(cellFun)) {
        if (cellFun[1] == 'o') {
          #outliers
          if (cfMat[i, j] > colMap[cmLen] || cfMat[i, j] < colMap[1]) {
            k <- ifelse(length(cellFun) > 1, cellFun[2], cfMat[i, j])
          }
        }
      } else if (is.function(cellFun)) {
        k <- cellFun(cfMat[i, j])
      } else if (is.list(cellFun)) {
        if (cellFun[[1]] == 'o') {
          #outliers
          if (cfMat[i, j] > colMap[cmLen] || cfMat[i, j] < colMap[1]) {
            k <- cellFun[-1]
          }
        }
      }

      if (is.numeric(k) && is.finite(k)) {
        nDig <- ifelse(abs(k) >= 10 ||
                         is.integer(k), 0, 1) #number decimal digits
        k <- format(round(k, digits = nDig), nsmall = nDig)
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



  # clustering and color dendrogram ----
  if (!is.null(nRowCluster)) {
    row_dend <- stats::as.dendrogram(stats::hclust(stats::dist(mat1))) # row clustering
    argList$cluster_rows <- dendextend::color_branches(row_dend, k = nRowCluster)
  }
  if (!is.null(nColmCluster)) {
    colm_dend <- stats::as.dendrogram(stats::hclust(stats::dist(t(mat1)))) # row clustering
    argList$cluster_columns <- dendextend::color_branches(colm_dend, k = nColmCluster)
  }


  # row annotation -----
  if (!is.null(rowAnnoDf)) {
    #rows must be at the same order as the row of mat1
    stopifnot(nrow(rowAnnoDf) == nrow(mat1))
    if (!is.null(rownames(rowAnnoDf)) && !is.null(rownames(mat1))) {
      stopifnot(all(rownames(rowAnnoDf) == rownames(mat1)))
    }

    colList <- list()
    for (i in colnames(rowAnnoDf)) {
      if (is.factor(rowAnnoDf[, i]))
        rowAnnoDf[, i] <- as.character(rowAnnoDf[, i])

      if (is.character(rowAnnoDf[, i])) {
        if (i %in% names(rowAnnoColMap)) {
          uniCols <- vecSwitch(rowAnnoColMap[[i]])
        } else{
          # handled here instead of passing to ComplexHeatmap::Heatmap() to
          # handle since the colorss it produces are not distinct enough.
          uniVals <- unique(rowAnnoDf[, i])
          uniVals[is.na(uniVals)] <- 'NA'
          #uniCols=randomcoloR::distinctColorPalette(length(uniVals),runTsne = T)
          uniCols <- getDistinctColors(length(uniVals))
          names(uniCols) <- uniVals
        }
        colList[[i]] <- uniCols
      } else if (is.numeric(rowAnnoDf[, i]) &&
                 i %in% names(rowAnnoColMap)) {
        colList[[i]] <- circlize::colorRamp2(rowAnnoColMap[[i]], names(rowAnnoColMap[[i]]))
      }
    }
    tmpList <- list(df = rowAnnoDf, col = colList)
    tmpList[names(rowAnnoPara)] <- rowAnnoPara
    row_anno <- do.call(ComplexHeatmap::rowAnnotation, tmpList)

    argList$left_annotation <- row_anno
  }


  # column annotation -----
  if (!is.null(colmAnnoDf)) {
    #columns must be at the same order as the columns of mat1
    stopifnot(nrow(colmAnnoDf) == ncol(mat1))
    if (!is.null(rownames(colmAnnoDf))) {
      stopifnot(all(rownames(colmAnnoDf) == colnames(mat1)))
    }

    colList <- list()
    for (i in colnames(colmAnnoDf)) {
      if (is.factor(colmAnnoDf[, i]) || is.logical(colmAnnoDf[, i]))
        colmAnnoDf[, i] <- as.character(colmAnnoDf[, i])

      if (is.character(colmAnnoDf[, i])) {
        if (i %in% names(colmAnnoColMap)) {
          uniCols <- vecSwitch(colmAnnoColMap[[i]])
        } else{
          uniVals <- unique(colmAnnoDf[, i])
          uniVals[is.na(uniVals)] <- 'NA'
          #uniCols=randomcoloR::distinctColorPalette(length(uniVals),runTsne = T)
          uniCols <- getDistinctColors(length(uniVals))
          names(uniCols) <- uniVals
        }
        colList[[i]] <- uniCols
      } else if (is.numeric(colmAnnoDf[, i]) &&
                 i %in% names(colmAnnoColMap)) {
        colList[[i]] <- circlize::colorRamp2(colmAnnoColMap[[i]], names(colmAnnoColMap[[i]]))
      }
    }
    tmpList <- list(df = colmAnnoDf, col = colList)
    tmpList[names(colmAnnoPara)] <- colmAnnoPara
    colm_anno <- do.call(ComplexHeatmap::columnAnnotation, tmpList)

    argList$top_annotation <- colm_anno
  }


  # combine all args and call Heatmap()----
  list1 <- c(
    list(title_position = "topleft",break_dist = legendBreakDist),
    legendBounds,
    argList$heatmap_legend_param
  )

  if(!is.null(legendHeight)) {
    list1$legend_height  <- grid::unit(legendHeight, 'cm')
  }

  argList$matrix <- mat0
  dotArgs <- list(...)
  dotArgs$heatmap_legend_param <- c(dotArgs$heatmap_legend_param, list1)

  # overwrite any hard-coded parameters with user supplied ones if any
  argList[names(dotArgs)] <- dotArgs
  ht1 <- do.call(ComplexHeatmap::Heatmap, argList)

  if (drawHeatmap) {
    ComplexHeatmap::draw(ht1,
                         merge_legend = TRUE,
                         heatmap_legend_side = legendPos[1])
  }else(ht1)
}
