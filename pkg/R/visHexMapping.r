#' Function to visualise various mapping items within a supra-hexagonal grid
#'
#' \code{visHexMapping} is supposed to visualise various mapping items within a supra-hexagonal grid
#'
#' @param sObj an object of class "sMap" or "sInit" or "sTopol"
#' @param mappingType the mapping type, can be "indexes", "hits", "dist", "antidist", "bases", and "customized"
#' @param labels NULL or a vector with the length of nHex
#' @param height a numeric value specifying the height of device
#' @param margin margins as units of length 4 or 1
#' @param area.size an inteter or a vector specifying the area size of each hexagon
#' @param gp an object of class "gpar". It is the output from a call to the function "gpar" (i.e., a list of graphical parameter settings)
#' @param border.color the border color for each hexagon
#' @param fill.color the filled color for each hexagon
#' @param clip either "on" for clipping to the extent of this viewport, "inherit" for inheriting the clipping region from the parent viewport, or "off" to turn clipping off altogether
#' @param newpage logical to indicate whether to open a new page. By default, it sets to true for opening a new page
#' @return 
#' invisible
#' @note The mappingType includes: 
#' \itemize{
#' \item{"indexes": the index of hexagons in a supra-hexagonal grid}
#' \item{"hits": the number of input data vectors hitting the hexagons}
#' \item{"dist": distance (in high-dimensional input space) to neighbors (defined in 2D output space)}
#' \item{"antidist": the oppose version of "dist"}
#' \item{"bases": clusters partitioned from the sMap}
#' \item{"customized": displaying input "labels"}
#' }
#' @export
#' @seealso \code{\link{sDmat}}, \code{\link{sDmatCluster}}, \code{\link{visHexGrid}}
#' @include visHexMapping.r
#' @examples
#' # 1) generate data with an iid matrix of 1000 x 9
#' data <- cbind(matrix(rnorm(1000*3,mean=0,sd=1), nrow=1000, ncol=3), 
#' matrix(rnorm(1000*3,mean=0.5,sd=1), nrow=1000, ncol=3), 
#' matrix(rnorm(1000*3,mean=-0.5,sd=1), nrow=1000, ncol=3))
#' colnames(data) <- c("S1","S1","S1","S2","S2","S2","S3","S3","S3")
#'
#' # 2) sMap resulted from using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) visualise supported mapping items within a supra-hexagonal grid
#' # 3a) for indexes of hexagons
#' visHexMapping(sMap,mappingType="indexes")
#' # 3b) for the number of input data vectors hitting the hexagons
#' visHexMapping(sMap,mappingType="hits")
#' # 3c) for distance (in high-dimensional input space) to neighbors (defined in 2D output space)
#' visHexMapping(sMap,mappingType="dist")
#' # 3d) for clusters/bases partitioned from the sMap
#' visHexMapping(sMap,mappingType="bases")

visHexMapping <- function (sObj, mappingType=c("indexes","hits","dist","antidist","bases","customized"), labels=NULL, height=7, margin=rep(0.1,4), area.size=1, gp=grid::gpar(cex=0.7, font=1, col.label="black"),  border.color="black", fill.color="transparent", clip=c("on","inherit","off"), newpage=T)
{
    
    mappingType <- match.arg(mappingType)
    
    if (class(sObj) != "sTopol" & class(sObj) != "sInit" & class(sObj) != "sMap"){
        stop("The funciton must apply to either 'sTopol' or 'sInit' or 'sMap' object.\n")
    }
    
    dat <- data.frame(sObj$coord)
    xdim <- sObj$xdim
    ydim <- sObj$ydim
    nHex <- sObj$nHex
    
    hbin <- hexbin::hexbin(dat$x, dat$y, xbins=xdim-1, shape=sqrt(0.75)*ydim/xdim)
        
    hbin@cell <- 1:nrow(dat)
    hbin@ncells <- nrow(dat)
    hbin@count <- rep(1,nrow(dat))
    hbin@xcm <- dat$x
    hbin@ycm <- dat$y
    
    if (newpage){
        #grid::grid.newpage()
        dev.new(width=height*xdim/ydim, height=height)
    }
    
    legend <- 0
    vp <- hexbin::hexViewport(hbin, offset=grid::unit(legend,"inches"), mar=grid::unit(margin,"lines"), xbnds=c(min(hbin@xcm)-0.5, max(hbin@xcm)+0.5), ybnds=c(min(hbin@ycm)-sqrt(0.75), max(hbin@ycm)+sqrt(0.75)))
    grid::pushViewport(vp@hexVp.off)
    
    xy <- list()
    xy$x <- dat$x
    xy$y <- dat$y
    
    if(mappingType == "indexes"){
        shape <- sObj$shape
        if(shape == "sheet"){
            labels <- 1:nrow(dat)
        }else if(shape=="suprahex"){
            r <- (xdim+1)/2
            nHex <- 1+6*r*(r-1)/2
            stepCentroid <- vector()
            stepCentroid[1] <- 1
            stepCentroid[2:nHex] <- unlist(sapply(2:r, function(x) (c( (1+6*x*(x-1)/2-6*(x-1)+1) : (1+6*x*(x-1)/2) )>=1)*x ))
            myColor <- c("#FFFFFF", "#888888")
            fill.color <- myColor[stepCentroid%%2 + 1]
            labels <- 1:nrow(dat)
        }
    }else if(mappingType == "hits"){
        if (class(sObj) != "sMap"){
            stop("The funciton with type 'hits' must apply to 'sMap' object.\n")
        }
        labels <- sObj$hits
        
        area.size <- log2(labels)
        area.size[!is.finite(area.size)] <- 0
        border.color <- "#888888"
        
    }else if(mappingType == "dist" | mappingType == "antidist"){
        if (class(sObj) != "sMap"){
            stop("The funciton with type 'dist' or 'antidist' must apply to 'sMap' object.\n")
        }
        ## calculate "median" distances in INPUT space to no more than 2-topological neighbors in 2D OUTPUT space
        dMat <- sDmat(sObj, which_neigh=2, distMeasure="median")
        area.size <- log2(dMat)
        if(mappingType == "antidist"){
            area.size <- -1*area.size
        }
        area.size[!is.finite(area.size)] <- 0
        border.color <- "#888888"
        
    }else if(mappingType == "bases"){
    
        if (class(sObj) != "sMap"){
            stop("The funciton with type 'bases' must apply to 'sMap' object.\n")
        }
        ## partition the grid map into clusters using region-growing algorithm with linkage "average"
        res <- sDmatCluster(sObj, which_neigh=2, distMeasure="median", clusterLinkage="average")
        labels <- rep("", length(res$bases))
        labels[res$seeds] <- as.character(seq(1,length(res$seeds)))
        myColor <- sample(rainbow(length(res$seeds)))
        fill.color <- myColor[res$bases]
        
        ## calculate "median" distances in INPUT space to no more than 2-topological neighbors in 2D OUTPUT space
        dMat <- sDmat(sObj, which_neigh=2, distMeasure="median")
        #area.size <- log2(dMat)
        area.size <- dMat
        area.size[!is.finite(area.size)] <- 0
        border.color <- "#888888"
        
    }else if(mappingType == "customized"){
        if(is.null(labels) | length(labels) != nHex){
            stop("The input for customized labels are wrong, please check the manual.\n")
        }
        
    }
    
    clip <- match.arg(clip)
    if (clip == "on") {
        grid::popViewport()
        grid::pushViewport(vp@hexVp.on)
    }
    
    visHexGrid(hbin, area.size=area.size, border.color=border.color, fill.color=fill.color)
    grid::grid.text(as.character(labels), xy$x, xy$y, gp=gp, default.units="native")
    
    invisible(vp)
}