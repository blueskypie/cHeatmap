## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
library(cHeatmap)

set.seed(100)
nMat1 <- rnorm(30)
uMat1 <- runif(12, -5, 20)
mat1 <- matrix(c(nMat1, uMat1), nrow = 6)

cHeatmap(mat1,
  name = "value", # legend title
  resetOutliers = F, # do not detect outliers
  cluster_rows = F, cluster_columns = F, # no clustering
  cellFun = function(x) { x }, # display all the values
  column_split = c(rep("nMat1", 5), rep("uMat1", 2)) # mark the two matrices
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  name = "value",
  resetOutliers = T,
  cluster_rows = F, cluster_columns = F,
  whiteValue = 0 # set the white values in the legend to 0
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  name = "value",
  resetOutliers = F,
  cluster_rows = F, cluster_columns = F,
  colMap = c(-1, 0, 3)
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  name = "value",
  resetOutliers = T,
  cluster_rows = F, cluster_columns = F,
  colMap = c(NA, 0, 3)
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  name = "value",
  resetOutliers = F,
  cluster_rows = F, cluster_columns = F,
  colMap = c("blue" = NA, "green" = 5, "red" = NA)
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  name = "value",
  resetOutliers = F,
  cluster_rows = F, cluster_columns = F,
  colMap = c("green" = -1, "green4" = 18)
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
# create an Inf value
mat2=mat1
mat2[2,2]=Inf

cHeatmap(mat2,
  name = "value",
  resetOutliers = F,
  colMap = c("green4" = -1, "white" = 0, "red" = 1, #cluster 1
             "yellow" = 3, "blue" = 18), #cluster 2
  column_split = c(rep("nMat1", 5), rep("uMat1", 2))
)
rm(mat2)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  name = "value",
  resetOutliers = F,
  colMap = c("green4" = -1, "white" = 0, "red" = 1, #cluster 1
             "yellow" = 3, "blue" = 18), #cluster 2
  column_split = c(rep("nMat1", 5), rep("uMat1", 2)),
  legendBreakDist = c(1, 1, 0.5, 3),
  legendHeight = 4
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
lt=c(-1,0,1,3,6,9,12,15,18)
cHeatmap(mat1,
         name = "value",
         resetOutliers = F,
         colMap = c(
           green = -1, green4 = 0, 
           red = 10e-6, red4=1,
           yellow=1+10e-6,yellow4 = 3, 
           blue=3+10e-6, blue4 = 18), 
         legendTicks = lt,
         legendBreakDist = rep.int(1,length(lt)-1),
         legendHeight = 4,
         column_split = c(rep("nMat1", 5), rep("uMat1", 2))
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  name = "value",
  resetOutliers = F,
  cluster_rows = F, cluster_columns = F,
  colMap = c(-1, 0, 3),
  cellFun = function(x) { if (x > 0.5 && x < 1) x }
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  name = "value",
  resetOutliers = F,
  cluster_rows = F, cluster_columns = F,
  colMap = c(-1, 0, 3),
  cellFun = "o" # 'o' is hard-coded to represent outliers
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1, cellFun = c("o", "*"))

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1, 
  cellFun = c(
    "o", list("rect", col = "black", lwd = 2)
  )
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  cellFun = function(x) {
    if (x > 2) {      "H"
    } else if (x < 0) "L"
  }
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  cellFun = function(x) {
    if (x > 2) {
      list("rect", col = "black", lwd = 2)
    } else if (x < 0) "L"
  }
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mat1,
  name = "value",
  resetOutliers = F,
  nRowCluster = 2, nColmCluster = 3,
  row_split = 2, column_split = 3
)

## ----fig.keep = c(1,3), fig.asp = 5/7-----------------------------------------
mati <- matrix(sample(1:100, 42, TRUE), nrow = 6)
cHeatmap(mati, name = "value")

mati <- matrix(sample(1:5, 42, TRUE), nrow = 6)
cHeatmap(mati, name = "value")

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(mati,
  name = "value",
  colMap = c("red" = 1, "pink" = 2, "yellow" = 3, "green" = 4, "green4" = 5),
  cellFun = function(x) {
    if (x %in% c(1, 3, 5)) x
  }
)

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
matc <- matrix(sample(letters[1:6], 42, TRUE), nrow = 6)
cHeatmap(matc, name = "value")

## ----fig.keep = 'last', fig.asp = 5/7-----------------------------------------
cHeatmap(matc,
  name = "value",
  nColmCluster = 3, nRowCluster = 2,
  colMap = c(
    "red" = "a", "pink" = "b", "yellow" = "c",
    "green" = "d", "green2" = "e", "green4" = "f"
  )
)

## ----fig.keep = 'last',fig.height=7,fig.width=10.5----------------------------
# create the matrix ----
mat1 <- matrix(sample(1:100, 42, TRUE), nrow = 6)
rownames(mat1) <- paste("row", 1:6)
colnames(mat1) <- paste("col", 1:7)

# create the row annotation data frame ---
rowDf <- data.frame(Sex = c("F", "F", "M", "M", "F", "M"), Age = 51:56)
rownames(rowDf) <- paste("row", 6:1)

# create the column annotation data frame ---
colDf <- data.frame(level = c("H", "H", "M", "M", "L", "L", "L"), order = 7:1)
rownames(colDf) <- paste("col", 7:1)

# make sure the rows and columns of the annotation data frame are at the same
#   order as those of mat1
stopifnot(all(rownames(mat1) %in% rownames(rowDf)) && nrow(mat1) == nrow(rowDf))
rowDf <- rowDf[rownames(mat1), ]
stopifnot(all(colnames(mat1) %in% rownames(colDf)) && ncol(mat1) == nrow(colDf))
colDf <- colDf[colnames(mat1), ]

cHeatmap(mat1, name = "value", rowAnnoDf = rowDf, colmAnnoDf = colDf)

## ----fig.keep = 'last', fig.height=7,fig.width=10.5---------------------------
cHeatmap(mat1,
  name = "value",
  rowAnnoDf = rowDf, colmAnnoDf = colDf,
  rowAnnoColMap = list(Sex = c("pink" = "F", "bisque" = "M")),
  colmAnnoColMap = list(
    level = c("red" = "H", "yellow" = "M", "green" = "L"),
    order = c("blue" = 1, "blue4" = 7)
  )
)

## ----fig.keep = 'last', fig.height=7,fig.width=10.5---------------------------
nMat1 <- matrix(rnorm(30), nrow = 6)
uMat1 <- matrix(runif(12, -5, 20), nrow = 6)

hm1 <- cHeatmap(nMat1, drawHeatmap = F, name = "nMat1")
hm2 <- cHeatmap(uMat1, drawHeatmap = F, name = "uMat1")
hm1 + hm2

## ----fig.keep = 'last', fig.height=7,fig.width=9.3,dpi=100--------------------
mat1 <- matrix(c(nMat1, uMat1), nrow = 6)
rownames(mat1) <- paste("pt", 1:6)
colnames(mat1) <- paste("visit", 1:7)

painScore <- rnorm(7)

cHeatmap(mat1,
  name = "value", cluster_columns = F,
  rowDraw = list(
    list("grid.lines", col = "black", lwd = 2),
    matrix(painScore, nrow = 1), # data to be plotted
    2 # plot at the 2nd row of mat1
  )
)

## ----fig.keep = 'last', fig.height=7,fig.width=9.3,dpi=100--------------------
# longitudinal scores of four patients
painScore <- matrix(rnorm(28), nrow = 4)
cHeatmap(mat1,
  name = "value", cluster_columns = F,
  rowDraw = list(
    list(
      list("grid.points", size = 0.5, pch = 15, col = "blue"),
      list("grid.lines", col = "black", lwd = 2)
    ),
    painScore, 
    c(2, 3, 4, 5) # row indices of the four patients in mat1
  )
)

