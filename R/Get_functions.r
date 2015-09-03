# utils to remove from
# within, leading and trailing spaces
ClearStrings<-function(query){
    # kill multiple white space
    query <- gsub(" +"," ",query);
    # remove leading and trailing space
    query<- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", query, perl=TRUE);
    return (query);
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