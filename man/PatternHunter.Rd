% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Correlations.R
\name{PatternHunter}
\alias{FeatureCorrelation}
\alias{Match.Pattern}
\alias{PatternHunter}
\title{Pattern hunter}
\usage{
FeatureCorrelation(dataSet, analSet, dist.name = "pearson", varName)

Match.Pattern(dataSet, analSet, dist.name = "pearson", pattern = NULL)
}
\arguments{
\item{dataSet}{List, data set object generated by \code{\link[MSdata]{MS_to_MA}} function.}

\item{analSet}{List, containing the results of statistical analysis (can be just an empty list).}

\item{dist.name}{Method of correlation calculating, one of \code{"pearson"}, \code{"kendall"}, \code{"spearman"}}

\item{varName}{The name of the feature to perform correlation analysis against.}

\item{pattern}{A character vector, the pattern to search. The pattern is specified as a series of numbers separated by "-". 
Each number corresponds to the expected expression pattern in the corresponding group. 
For example, a 1-2-3-4 pattern is used to search for features that increase linearly with time 
in a time-series data with four time points (or four groups). 
The order of the groups is given as the first item in the predefined patterns.}
}
\value{
Native \code{analSet} with one added \code{$corr} element consisting of
\itemize{
\item\code{$corr$sig.nm} - ???
\item\code{$corr$cor.mat} - correlation matrix
\item\code{$corr$pattern} - name of the feature of comparison or the used pattern.
}
}
\description{
Correlation analysis.

\code{FeatureCorrelation} performs analysis against a given feature.
Writes an output file \code{"correlation_feature.csv"}

\code{Match.Pattern} performs analysis against a given pattern. 
Writes an output file \code{"correlation_pattern.csv"} respectively.
}
\seealso{
\code{\link{PlotCorr}}, \code{\link{PlotCorrHeatMap}} for plotting functions

\code{\link{PlotCorr}}, \code{\link{PlotCorrHeatMat}}
}

