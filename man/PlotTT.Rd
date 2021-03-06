% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ttests.R
\name{PlotTT}
\alias{PlotTT}
\title{Plot t-test results}
\usage{
PlotTT(dataSet, analSet, imgName = "tt_", format = "png", dpi = 72,
  width = NA)
}
\arguments{
\item{dataSet}{List, data set object generated by \code{\link[MSdata]{MS_to_MA}} function.}

\item{analSet}{List, containing the results of statistical analysis (can be just an empty list).}

\item{imgName}{Image file name prefix.}

\item{format}{Image format, one of: "png", "tiff", "pdf", "ps", "svg"}

\item{dpi}{Image resolution.}

\item{width}{Image width.}
}
\description{
Plot t-test results
}
\seealso{
\code{\link{Ttests.Anal}} for analytical function
}

