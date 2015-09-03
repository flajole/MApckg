#' Permutation PLSDA test
#'
#' Perform permutation, using training classification accuracy or separation distance as
#' indicator, for two or multi-groups.
#' @param dataSet
#' @param analSet
#' @param num The number of permutations.
#' @param type The indicator. If \code{"accu"} - training classification accuracy; 
#' if \code{"sep"} - separation distance.
#' @return Native \code{analSet} with some elements added:
#' \itemize{
#' \item\code{$plsda$permut.p} - 
#' \item\code{$plsda$permut.inf} - 
#' \item\code{$plsda$permut.type} - 
#' \item\code{$plsda$permut} - 
#' }
#' @export

# perform permutation, using training classification accuracy as
# indicator, for two or multi-groups
PLSDA.Permut<-function(dataSet, analSet, num=100, type="accu"){
	if (is.null(analSet$plsr)) stop("Please, conduct PLS.Anal and PLSDA.CV first.")
	if (is.null(analSet$plsda)) stop("Please, conduct PLSDA.CV first.")
	match.arg(type, c("accu", "sep"))
    orig.cls<-cls<-as.numeric(dataSet$cls);
    datmat<-as.matrix(dataSet$norm);
    best.num<-analSet$plsda$best.num;

    # dummy is not used, for the purpose to maintain lapply API
    Get.pls.bw <- function(dummy){
         cls <- cls[order(runif(length(cls)))];
         pls <- plsda(datmat, as.factor(cls), ncomp=best.num);
         pred <- predict(pls, datmat);
         Get.bwss(pred, cls);
    }

    Get.pls.accu <- function(dummy){
         cls <- cls[order(runif(length(cls)))];
         pls <- plsda(datmat, as.factor(cls), ncomp=best.num);
         pred <- predict(pls, datmat);
         sum(pred == cls)/length(cls);
    }

    # first calculate the bw values with original labels
    pls <- plsda(datmat, as.factor(orig.cls), ncomp=best.num);
    pred.orig <- predict(pls, datmat);
    if(type=="accu"){
        perm.type = "prediction accuracy";
        res.orig <- sum(pred.orig == orig.cls)/length(orig.cls);
        res.perm <- Perform.permutation(num, Get.pls.accu);
    }else{
        perm.type = "separation distance";
        res.orig <- Get.bwss(pred.orig, orig.cls);
        res.perm <- Perform.permutation(num, Get.pls.bw);
     }

    perm.vec <- c(res.orig, unlist(res.perm, use.names=FALSE));
    # check for infinite since with group variance could be zero for perfect classification
    inf.found = TRUE;
	if(sum(is.finite(perm.vec))==length(perm.vec)){
    		inf.found = FALSE;
	}else {
	 	if(sum(is.finite(perm.vec))==0){ # all are infinite, give a random number 10
			perm.vec<-rep(10, length(perm.vec));
		}else{ # if not all inf, replace with the 10 fold of non-inf values
    		perm.vec[!is.finite(perm.vec)]<-10*max(perm.vec[is.finite(perm.vec)]);
		}
	}

    # calculate the significant p value as the proportion of sampled permutations better than or equal to original one
    # note, the precision is determined by the permutation number i.e. for 100 time, no better than original
    # p value is < 0.01, we can not say it is zero
    better.hits <- sum(perm.vec[-1]>=perm.vec[1]);
    if(better.hits == 0) {
        p <- paste("p < ", 1/num, " (", better.hits, "/", num, ")", sep="");
    }else{
        p <- better.hits/num;
        p <- paste("p = ", signif(p, digits=5), " (", better.hits, "/", num, ")", sep="");
    }

    analSet$plsda$permut.p<-p;
    analSet$plsda$permut.inf<-F;
    analSet$plsda$permut.type<- perm.type;
    analSet$plsda$permut<-perm.vec;
	print(p);
    return(analSet);
}

#' \code{PlotPLS.Permutation} - plot PLSDA permutation plot.
#' @rdname PlotPLS
#' @export

# Plot plsda classification performance using different components
PlotPLS.Permutation<-function(dataSet, analSet, imgName="pls_perm_", format="png", dpi=72, width=NA){
	if (is.null(analSet$plsr)) stop("Please, conduct PLS.Anal, PLSDA.CV and PLSDA.Permutation first.")
	if (is.null(analSet$plsda)) stop("Please, conduct PLSDA.CV and PLSDA.Permutation first.")
	if (is.null(analSet$plsda$permut)) stop("Please, conduct PLSDA.Permutation first.")
	bw.vec<-analSet$plsda$permut;
    len<-length(bw.vec);

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        w <- 8;
    }else if(width == 0){
        w <- 7;
        imgSet$pls.permut<<-imgName;
    }else{
        w <- width; 
    }
    h <- w*6/8;

    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mar=c(5,5,2,4));
    hst <- hist(bw.vec, breaks = "FD", freq=T,
            ylab="Frequency", xlab= 'Permutation test statistics', col="lightblue", main="");

    # add the indicator using original label
    h <- max(hst$counts)
    arrows(bw.vec[1], h/5, bw.vec[1], 0, col="red", lwd=2);
    text(bw.vec[1], h/3.5, paste('Observed \n statistic \n', analSet$plsda$permut.p), xpd=T);
    dev.off();
	frame()
	grid::grid.raster(png::readPNG(imgName));
}


# Compute BSS/WSS for each row of a matrix which may have NA
# Columns have labels
# x is a numeric vector,
# cl is consecutive integers

Get.bwss<-function(x, cl){
   K <- max(cl) - min(cl) + 1
   tvar <- var.na(x);
   tn <- sum(!is.na(x));
   wvar <- wn <- numeric(K);

   for(i in (1:K)) {
     if(sum(cl == (i + min(cl) - 1)) == 1){
        wvar[i] <- 0;
        wn[i] <- 1;
     }

     if(sum(cl == (i + min(cl) - 1)) > 1) {
        wvar[i] <- var.na(x[cl == (i + min(cl) - 1)]);
        wn[i] <- sum(!is.na(x[cl == (i + min(cl) - 1)]));
     }
   }

   WSS <- sum.na(wvar * (wn - 1));
   TSS <- tvar * (tn - 1)
   (TSS - WSS)/WSS;
}

# use single core on the public server
Perform.permutation <- function(perm.num, fun){
   print(paste("performing", perm.num, "permutations ..."));
   #suppressMessages(require('multicore'));
   #core.num <- multicore:::detectCores();

   #if(core.num > 1){ # use two CPUs only, otherwise, the server will be unresponsive for other users
   #    perm.res <- mclapply(2:perm.num, fun, mc.cores =core.num-1);
   #}else{ # just regular
       perm.res <- lapply(2:perm.num,fun);
   #}

   perm.res;
}