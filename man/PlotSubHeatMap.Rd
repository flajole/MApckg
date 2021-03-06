% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HeatMap.R
\name{PlotSubHeatMap}
\alias{PlotSubHeatMap}
\title{Plot heatmap}
\usage{
PlotSubHeatMap(dataSet, analSet, imgName = "heatmap_", format = "png",
  dpi = 72, width = NA, smplDist = "euclidean", clstDist = "ward.D",
  colors = "default", method.nm = "tanova", top.num = 25,
  viewOpt = "overview", rowV = TRUE, colV = TRUE, border = T)
}
\arguments{
\item{dataSet}{List, data set object generated by \code{\link[MSdata]{MS_to_MA}} function.}

\item{analSet}{List, containing the results of statistical analysis (can be just an empty list).}

\item{imgName}{Image file name prefix.}

\item{format}{Image format, one of: "png", "tiff", "pdf", "ps", "svg"}

\item{dpi}{Image resolution.}

\item{width}{Image width.}

\item{smplDist}{The distance measure, one of \code{"euclidean"}, \code{"pearson"}, \code{"minkowski"}}

\item{clstDist}{The agglomeration method to be used, one of "ward.D", "ward.D2", "single", "complete", "average".
For details: \code{\link[pheatmap]{pheatmap}}}

\item{colors}{The color contrast. One of \code{"default"}, \code{"gbr"} (red/green), \code{"heat"}, \code{"topo"}, \code{"gray"}}

\item{method.nm}{Type of analysis applied to detect features with the best contrast. One of \code{"tanova"} - for t-test or ANOVA;
\code{"vip"} - for PLS-DA VIP; \code{"rf"} - for Random Forest analysis}

\item{top.num}{The number of the features with the best contrast to be plotted.}

\item{viewOpt}{View mode, \code{"overview"} or \code{"detailed"}}

\item{rowV}{If \code{TRUE}, samples are reorganized.}

\item{colV}{If \code{TRUE}, features are reorganized.}

\item{border}{If \code{TRUE}, show cell borders.}
}
\value{
Native \code{analSet} with one added \code{$htmap} element containing:
\itemize{
\item\code{$htmap$dist.par} - equal \code{smplDist} argument value
\item\code{$htmap$clust.par} - equal \code{clstDist} argument value  
}
}
\description{
Plot a sub heatmap based on results from t-tests/ANOVA, VIP or Random Forest analysis.
Uses \code{\link[pheatmap]{pheatmap}} function.
Heatmap provides intuitive visualization of the data table. 
Each colored cell on the map corresponds to a concentration value in your data table, 
with samples in rows and features/compounds in columns. 
You can use heatmap to identify samples/features that are unusually high/low. \cr
Tip 1: Do not re-organize samples/rows to show the natural contrast among groups (with each group a block). \cr
Tip 2: Display top number of features ranked by t-tests to retain the most constrasting patterns.
}
\seealso{
\code{\link[pheatmap]{pheatmap}} for used statistical function\cr
\code{\link{PlotHeatMap2}} for two-factored data
}

