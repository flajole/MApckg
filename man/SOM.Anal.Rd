% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cluster_SOM.R
\name{SOM.Anal}
\alias{SOM.Anal}
\title{SOM analysis}
\usage{
SOM.Anal(dataSet, analSet, x.dim = 1, y.dim = 3, initMethod = "linear",
  neigb = "gaussian")
}
\arguments{
\item{dataSet}{List, data set object generated by \code{\link[MSdata]{MS_to_MA}} function.}

\item{analSet}{List, containing the results of statistical analysis (can be just an empty list).}

\item{x.dim}{An integer specifying the x-dimension of the map.}

\item{y.dim}{An integer specifying the y-dimension of the map.}

\item{initMethod}{A character string specifying the initializing method. 
The following are permitted: \code{"sample"} uses a random sample from the data; 
\code{"random"} uses random draws from N(0,1); 
\code{"linear"} uses the linear grids upon the first two principle components directin.}

\item{neigb}{A character string specifying the neighborhood function type.
The following are permitted: \code{"bubble"}, \code{"gaussian"}}
}
\value{
Native \code{analSet} with one added \code{$som} element containing
standard \code{\link[som]{som}} output.
}
\description{
Perform Self Organizing Map analysis. Uses \code{\link[som]{som}} function.
The parameters and outcome are identical.
}
\seealso{
\code{\link{PlotSOM}} for plotting functions
}

