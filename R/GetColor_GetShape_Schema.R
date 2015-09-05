GetShapeSchema <- function(dataSet, show.name, grey.scale, shapeVec = NULL){
    if(!is.null(shapeVec) && all(shapeVec > 0)){
        sps <- rep(0, length=length(dataSet$cls));
        clsVec <- as.character(dataSet$cls)
        grpnms <- names(shapeVec);
        for(i in 1:length(grpnms)){
            sps[clsVec == grpnms[i]] <- shapeVec[i];
        }
        shapes <- sps;
    }else{
        if(show.name | grey.scale){
            shapes <- as.numeric(dataSet$cls)+1;
        }else{
            shapes <- rep(19, length(dataSet$cls));
        }
    }
    return(shapes);
}

GetColorSchema <- function(dataSet, grayscale = FALSE, colVec = NULL){
    # test if total group number is over 9
     grp.num <- length(levels(dataSet$cls));

     if(grayscale){
        dist.cols <- grDevices::colorRampPalette(c("grey90", "grey30"))(grp.num);
        lvs <- levels(dataSet$cls);
        colors <- vector(mode="character", length=length(dataSet$cls));
        for(i in 1:length(lvs)){
            colors[dataSet$cls == lvs[i]] <- dist.cols[i];
        }
     }else if(grp.num > 9){
        pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
                    "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
                    "#FFFF99", "#B15928");
        dist.cols <- grDevices::colorRampPalette(pal12)(grp.num);
        lvs <- levels(dataSet$cls);
        colors <- vector(mode="character", length=length(dataSet$cls));
        for(i in 1:length(lvs)){
            colors[dataSet$cls == lvs[i]] <- dist.cols[i];
        }
     }else{
        if(!is.null(colVec) && !any(colVec =="#NA")){
            cols <- vector(mode="character", length=length(dataSet$cls));
            clsVec <- as.character(dataSet$cls)
            grpnms <- names(colVec);
            for(i in 1:length(grpnms)){
                cols[clsVec == grpnms[i]] <- colVec[i];
            }
            colors <- cols;
        }else{
            colors <- as.numeric(dataSet$cls)+1;
        }
     }
    return (colors);
}