#' K-means analysis
#'
#' Perform K-means analysis. Uses \code{\link[stats]{kmeans}} function.
#' @param dataSet List, data set object generated by \code{\link[MSdata]{MS_to_MA}} function.
#' @param analSet List, containing the results of statistical analysis (can be just an empty list).
#' @param clust.num The cluster number. 
#' @return Native \code{analSet} with one added \code{$kmeans} element containing
#' standard \code{\link[stats]{kmeans}} output.
#' @seealso \code{\link{PlotKmeans}} for plotting functions
#' @export
# functions for k-means analysis
Kmeans.Anal<-function(dataSet, analSet, clust.num = 3){
	analSet$kmeans<-kmeans (dataSet$norm, clust.num, nstart=100);
	return(analSet);
}

#' Plot K-means
#'
#' Plot K-means map.
#' Please note: only the cluster members will be calculated
#' if the specified cluster number > 20. 
#' The blue lines represent the median intensities of each cluster.
#' @param dataSet List, data set object generated by \code{\link[MSdata]{MS_to_MA}} function.
#' @param analSet List, containing the results of statistical analysis (can be just an empty list). 
#' @param imgName Image file name prefix.
#' @param format Image format, one of: "png", "tiff", "pdf", "ps", "svg"
#' @param dpi Image resolution.
#' @param width Image width.
#' @seealso \code{\link{Kmeans.Anal}} for analytical function
#' @export
PlotKmeans<-function(dataSet, analSet, imgName="kmeans_", format="png", dpi=72, width=NA){
	
	if (is.null(analSet$kmeans)) stop("Please, conduct Kmeans.Anal first.")
	clust.num <- max(analSet$kmeans$cluster);

    if(clust.num>20) return();
	# calculate arrangement of panel
    ylabel<-GetValueLabel(dataSet);
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 9;
    }else if(width == 0){
        w <- 7;
        analSet$imgSet$kmeans<-imgName;
    }else{
        w <- width;
    }
    h <- w*8/9;

    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mfrow = GetXYCluster(clust.num), mar=c(5,4,2,2));
	for (loop in 1:clust.num) {
	    matplot(t(dataSet$norm[analSet$kmeans$cluster==loop,]), type="l", col='grey', ylab=ylabel, axes=F,
            main=paste("Cluster ",loop, ", n=", analSet$kmeans$size[loop], sep=""))
            lines(apply(dataSet$norm[analSet$kmeans$cluster==loop,], 2, median), type="l", col='blue', lwd=1);
        axis(2);
        axis(1, 1:ncol(dataSet$norm), substr(colnames(dataSet$norm), 1, 7), las=2);
	}
    dev.off();
	frame()
	grid::grid.raster(png::readPNG(imgName));
}

# get cluster member for give index
# add HTML color to the names based on its group membership
GetKMClusterMembers<-function(dataSet, analSet, i){
       all.cols <- GetColorSchema(dataSet);
       hit.inx <- analSet$kmeans$cluster== i;

       paste("<font color=\"", all.cols[hit.inx], "\">", rownames(dataSet$norm)[hit.inx], "</font>",collapse =", ");
       # paste(all.cols[hit.inx], rownames(dataSet$norm)[hit.inx], collapse =", ");
}

GetAllKMClusterMembers<-function(dataSet, analSet){
	clust.df = data.frame();
	rowNameVec = c();
	i = 1;
	clust.num<-max(analSet$kmeans$cluster);
	while(i<=clust.num){
		if(i==1){
			clust.df <- rbind(paste(rownames(dataSet$norm)[analSet$kmeans$cluster== i], collapse = " "));
		}else{
			clust.df <- rbind(clust.df,paste(rownames(dataSet$norm)[analSet$kmeans$cluster== i], collapse = " "));
		}
		rowNameVec <- c(rowNameVec, paste("Cluster(", i, ")"));
		i = i+1;
	}
	row.names(clust.df)<- rowNameVec;
	colnames(clust.df)<-"Samples in each cluster";
	print(xtable::xtable(clust.df, align="l|p{8cm}", caption="Clustering result using K-means"), caption.placement="top", size="\\scriptsize");
}
