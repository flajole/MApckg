sum.na <- function(x,...){
     res <- NA
     tmp <- !(is.na(x) | is.infinite(x))
     if(sum(tmp) > 0)
           res <- sum(x[tmp])
     res
}

var.na <- function(x){
        res <- NA
        tmp <- !(is.na(x) | is.infinite(x))
        if(sum(tmp) > 1){
              res <- var(x[tmp])
         }
        res
}

# test if a sig table matrix is empty
isEmptyMatrix<-function(mat){
    if(is.null(mat) | length(mat)==0){
        return(TRUE);
    }
    if(nrow(mat)==0 | ncol(mat)==0){
        return(TRUE);
    }
    if(is.na(mat[1,1])){
        return(TRUE);
    }
    return(FALSE);
}

# utils to remove from
# within, leading and trailing spaces
ClearStrings<-function(query){
    # kill multiple white space
    query <- gsub(" +"," ",query);
    # remove leading and trailing space
    query<- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", query, perl=TRUE);
    return (query);
}

# replace -Inf, Inf to 99999 and -99999
CleanNumber <-function(bdata){
    if(sum(bdata==Inf)>0){
        inx <- bdata == Inf;
        bdata[inx] <- NA;
        bdata[inx] <- 999999;
    }
    if(sum(bdata==-Inf)>0){
        inx <- bdata == -Inf;
        bdata[inx] <- NA;
        bdata[inx] <- -999999;
    }
    bdata;
}

# determine value label for plotting
GetValueLabel<-function(dataSet){
	if(dataSet$type=="conc"){
	    return("Concentration");
	}else {
	    return("Intensity");
	}
}

# determine variable label for plotting
GetVariableLabel<-function(dataSet){
	if(dataSet$type=="conc"){
	    return("Compounds");
	}else if(dataSet$type=="specbin"){
	    return("Spectra Bins");
	}else if(dataSet$type=="nmrpeak"){
	    return("Peaks (ppm)");
	}else if(dataSet$type=="mspeak"){
        if(dataSet$peakSet$ncol==2){
            return("Peaks (mass)");
        }else{
            return("Peaks (mz/rt)");
        }
    }else{
        return("Peaks(mz/rt)");
    }
}

GetGroupNumber<-function(dataSet){
    return(dataSet$cls.num);
}

IsSmallSmplSize<-function(dataSet){
    return(dataSet$small.smpl.size);
}

GetMinGroupSize<-function(dataSet){
    return(dataSet$min.grp.size);
}

IsDataContainsNegative<-function(dataSet){
    return(dataSet$containsNegative);
}

# extend the axis range to both end
# vec is the values for that axis
# unit is the width to extend, 10 will increase by 1/10 of the range
GetExtendRange<-function(vec, unit=10){
    var.max <- max(vec, na.rm=T);
    var.min <- min(vec, na.rm=T);
    exts <- (var.max - var.min)/unit;
    c(var.min-exts, var.max+exts);
}

# generate Latex table
GetSigTable<-function(dataSet, mat, method){
    if(!isEmptyMatrix(mat)){ # test if empty
        cap<-"Important features identified by";
        if(nrow(mat)>50){
            smat<-as.matrix(mat[1:50,]); # only print top 50 if too many
            colnames(smat)<-colnames(mat); # make sure column names are also copied
            mat<-smat;
            cap<-"Top 50 features identified by";
        }
        # change the rowname to first column
        col1<-rownames(mat);
        cname<-colnames(mat);
        cname<-c(GetVariableLabel(dataSet), cname);
        mat<-cbind(col1, mat);
        rownames(mat)<-NULL;
        colnames(mat)<-cname;
        print(xtable::xtable(mat, caption=paste(cap, method)), ,caption.placement="top", size="\\scriptsize");
    }else{
        print(paste("No significant features were found using the given threshold for", method));
    }
}

