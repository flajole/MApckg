#' Set color schema
#' 
#' @export

SetColor <- function(dataSet, colVec, msdata, fac) {
    
    fac.match <- charmatch(fac, names(msdata@sampleData))
    if (is.na(fac.match))
        stop("Factor ", fac, " is not found. Please, check sample data.");
    if (fac.match == 0)
        stop("Multiple factors matching ", fac, " are found. Please, check sample data.");
    
    fac <- as.factor(msdata@sampleData[[fac.match]])
    fac <- factor(fac, fac[sort(match(levels(fac), fac))])
    grp.num <- length(levels(fac))
    if (is.null(colVec)) {
        if (grp.num > 9) {
            colVec <- grDevices::colorRampPalette(c("#A6CEE3", "#1F78B4", "#B2DF8A",
                                                    "#33A02C", "#FB9A99", "#E31A1C",
                                                    "#FDBF6F", "#FF7F00", "#CAB2D6",
                                                    "#6A3D9A", "#FFFF99", "#B15928"))(grp.num)
        } else {
            colVec <- c(1:grp.num) + 1
        }
    } else if (length(colVec) != grp.num) {
        stop("Length of color vector is not equal to the number of factor levels.")
    }
    
    levels(fac) <- colVec
    dataSet$colVec <- as.character(fac)
    levels(fac) <- grDevices::colorRampPalette(c("grey90", "grey30"))(grp.num)
    dataSet$greyVec <- as.character(fac)
    return(dataSet)
}

#' Set shape schema
#' 
#' @export

SetShape <- function(dataSet, shapeVec, msdata, fac) {
    
    fac.match <- charmatch(fac, names(msdata@sampleData))
    if (is.na(fac.match))
        stop("Factor ", fac, " is not found. Please, check sample data.");
    if (fac.match == 0)
        stop("Multiple factors matching ", fac, " are found. Please, check sample data.");
    
    fac <- as.factor(msdata@sampleData[[fac.match]])
    fac <- factor(fac, fac[sort(match(levels(fac), fac))])
    grp.num <- length(levels(fac))
    
    if (is.null(shapeVec)) {
        shapeVec <- c(1:grp.num) + 1
    } else if (length(shapeVec) != grp.num) {
        stop("Length of shape vector is not equal to the number of factor levels.")
    } else if (any(shapeVec < 0) || any(shapeVec > 25)) {
        stop("Shape values should be from 0 to 25.")
    }
    
    levels(fac) <- shapeVec
    dataSet$shapeVec <- as.integer(as.character(fac))
    return(dataSet)
}

GetShapeSchema <- function(dataSet, show, gray.scale){
    if (gray.scale && show) {
        shapeVec <- rep(19, length(dataSet$cls))
    } else {
        shapeVec <- dataSet$shapeVec;
    }
    return(shapeVec);
}

GetColorSchema <- function(dataSet, gray.scale = FALSE){
    if (gray.scale){
        colVec <- dataSet$grayVec
    } else {
        colVec <- dataSet$colVec
    }
    return (colVec);
}
