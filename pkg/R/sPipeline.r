#' Function to setup the pipeline for completing ab initio training given the input data
#'
#' \code{sPipeline} is supposed to finish ab inito training for the input data. It returns an object of class "sMap". 
#'
#' @param data a data frame or matrix of input data
#' @param xdim an integer specifying x-dimension of the grid
#' @param ydim an integer specifying y-dimension of the grid
#' @param nHex the number of hexagons/rectangles in the grid
#' @param lattice the grid lattice, either "hexa" for a hexagon or "rect" for a rectangle
#' @param shape the grid shape, either "suprahex" for a supra-hexagonal grid or "sheet" for a hexagonal/rectangle sheet
#' @param init an initialisation method. It can be one of "uniform", "sample" and "linear" initialisation methods
#' @param algorithm the training algorithm. It can be one of "sequential" and "batch" algorithm
#' @param alphaType the alpha type. It can be one of "invert", "linear" and "power" alpha types
#' @param neighKernel the training neighborhood kernel. It can be one of "gaussian", "bubble", "cutgaussian", "ep" and "gamma" kernels
#' @param finetuneSustain logical to indicate whether sustain the "finetune" training. If true, it will repeat the "finetune" stage until the mean quantization error does get worse. By default, it sets to true
#' @param verbose logical to indicate whether the messages will be displayed in the screen. By default, it sets to false for no display
#' @return 
#' an object of class "sMap", a list with following components:
#'  \item{nHex}{the total number of hexagons/rectanges in the grid}
#'  \item{xdim}{x-dimension of the grid}
#'  \item{ydim}{y-dimension of the grid}
#'  \item{lattice}{the grid lattice}
#'  \item{shape}{the grid shape}
#'  \item{coord}{a matrix of nHex x 2, with rows corresponding to the coordinates of all hexagons/rectangles in the 2D map grid}
#'  \item{init}{an initialisation method}
#'  \item{neighKernel}{the training neighborhood kernel}
#'  \item{codebook}{a codebook matrix of nHex x ncol(data), with rows corresponding to prototype vectors in input high-dimensional space}
#'  \item{hits}{a vector of nHex, each element meaning that a hexagon/rectangle contains the number of input data vectors being hit wherein}
#'  \item{mqe}{the mean quantization error for the "best" BMH}
#'  \item{call}{the call that produced this result}
#' @note The pipeline sequentially consists of: 
#' \itemize{
#' \item{i) \code{\link{sTopology}} used to define the topology of a grid (with "suprahex" shape by default ) according to the input data;}
#' \item{ii) \code{\link{sInitial}} used to initialise the codebook matrix given the pre-defined topology and the input data (by default using "uniform" initialisation method);}
#' \item{iii) \code{\link{sTrainology}} and \code{\link{sTrainSeq}} used to get the grid map trained at both "rough" and "finetune" stages. If instructed, sustain the "finetune" training until the mean quantization error does get worse;}
#' \item{iv) \code{\link{sBMH}} used to identify the best-matching hexagons/rectangles (BMH) for the input data, and these response data are appended to the resulting object of "sMap" class.}
#' }
#' @export
#' @seealso \code{\link{sTopology}}, \code{\link{sInitial}}, \code{\link{sTrainology}}, \code{\link{sTrainSeq}}, \code{\link{sTrainBatch}}, \code{\link{sBMH}}, \code{\link{visHexMulComp}}
#' @include sPipeline.r
#' @references
#' Hai Fang and Julian Gough. (2013). supraHex: an R/Bioconductor package for tabular omics data analysis using a supra-hexagonal map. \emph{Biochemical and Biophysical Research Communications}, \url{http://dx.doi.org/10.1016/j.bbrc.2013.11.103}.
#' @author Hai Fang \email{hfang@@cs.bris.ac.uk}
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#' colnames(data) <- paste(rep('S',10), seq(1:10), sep="")
#'
#' # 2) get trained using by default setup but with different neighborhood kernels
#' # 2a) with "gaussian" kernel
#' sMap <- sPipeline(data=data, neighKernel="gaussian")
#' # 2b) with "bubble" kernel
#' # sMap <- sPipeline(data=data, neighKernel="bubble")
#' # 2c) with "cutgaussian" kernel
#' # sMap <- sPipeline(data=data, neighKernel="cutgaussian")
#' # 2d) with "ep" kernel
#' # sMap <- sPipeline(data=data, neighKernel="ep")
#' # 2e) with "gamma" kernel
#' # sMap <- sPipeline(data=data, neighKernel="gamma")
#' 
#' # 3) visualise multiple component planes of a supra-hexagonal grid
#' visHexMulComp(sMap, colormap="jet", ncolors=20, zlim=c(-1,1), gp=grid::gpar(cex=0.8))

sPipeline <- function(data=NULL, xdim=NULL, ydim=NULL, nHex=NULL, lattice=c("hexa","rect"), shape=c("suprahex","sheet"), init=c("linear","uniform","sample"), algorithm=c("batch","sequential"), alphaType=c("invert","linear","power"), neighKernel=c("gaussian","bubble","cutgaussian","ep","gamma"), finetuneSustain=F, verbose=T)
{

    startT <- Sys.time()
    message(paste(c("Start at ",as.character(startT)), collapse=""), appendLF=T)
    message("", appendLF=T)
    ####################################################################################
    
    ## match.arg matches arg against a table of candidate values as specified by choices, where NULL means to take the first one
    lattice <- match.arg(lattice)
    shape <- match.arg(shape)
    init <- match.arg(init)
    algorithm <- match.arg(algorithm)
    alphaType <- match.arg(alphaType)
    neighKernel <- match.arg(neighKernel)
    
    ## define the topology of a map grid
    if (verbose) message("First, define topology of a map grid...", appendLF=T)
    sTopol <- sTopology(data=data, xdim=xdim, ydim=ydim, nHex=nHex, lattice=lattice, shape=shape)
    
    ## initialise the codebook matrix given a topology and input data
    if (verbose) message("Second, initialise the codebook matrix given a topology and input data...", appendLF=T)
    sI <- sInitial(data=data, sTopol=sTopol, init=init) 
    
    ## get training at the rough stage
    if (verbose) message("Third, get training at the rough stage...", appendLF=T)
    sT_rough <- sTrainology(sMap=sI, data=data, algorithm=algorithm, stage="rough", alphaType=alphaType, neighKernel=neighKernel)
    if(algorithm == "sequential"){
        sM_rough <- sTrainSeq(sMap=sI, data=data, sTrain=sT_rough)
    }else{
        sM_rough <- sTrainBatch(sMap=sI, data=data, sTrain=sT_rough)
    }

    ## get training at the finetune stage
    if (verbose) message("Fourth, get training at the finetune stage...", appendLF=T)
    sT_finetune <- sTrainology(sMap=sI, data=data, algorithm=algorithm, stage="finetune", alphaType=alphaType, neighKernel=neighKernel)
    if(algorithm == "sequential"){
        sM_finetune <- sTrainSeq(sMap=sM_rough, data=data, sTrain=sT_finetune)
    }else{
        sM_finetune <- sTrainBatch(sMap=sM_rough, data=data, sTrain=sT_finetune)
    }
    
    if(finetuneSustain){
        ## identify the best-matching hexagon/rectangle for the input data
        ##cat("Identify the best-matching hexagon/rectangle for the input data...\n",append=F)
        response <- sBMH(sMap=sM_finetune, data=data, which_bmh="best")
        # bmh: the requested BMH matrix of dlen x length(which_bmh)
        # qerr: the corresponding matrix of quantization errors
        # mqe: average quantization error
    
        ## sustain the finetune training till the mean quantization error (mqe) does not get worsen
        if (verbose) message("Fifth, sustain the next 10 rounds of finetune training till the mean quantization error (mqe) does get worse...", appendLF=T)
        mqe <- vector()
        k=1
        mqe[k] <- round(response$mqe * 10)/10
    
        if(verbose){
            message <- paste(c("\t", k, " iteration ", "with current mqe=", mqe[k]), collapse="")
            message(message, appendLF=T)
        }
    
        sM_now <- sM_finetune
        flag <- 1
        while(flag){
            sM_pre <- sM_now
        
            if(algorithm == "sequential"){
                sM_now <- sTrainSeq(sMap=sM_pre, data=data, sTrain=sT_finetune)
            }else{
                sM_now <- sTrainBatch(sMap=sM_pre, data=data, sTrain=sT_finetune)
            }
            response <- sBMH(sMap=sM_now, data=data, which_bmh="best")

            k <- k+1
            mqe[k] <- round(response$mqe * 10)/10
            if((mqe[k] >= mqe[k-1]) | k == 10){
                flag <- 0
            }
        
            if(verbose){
                message <- paste(c("\t", k, " iteration ", "with current mqe=", mqe[k]), collapse="")
                message(message, appendLF=T)
            }
        }
        
        sM_final <- sM_pre
    }else{
        sM_final <- sM_finetune
    }
    
    if (verbose) message("Next, identify the best-matching hexagon/rectangle for the input data...", appendLF=T)
    response <- sBMH(sMap=sM_final, data=data, which_bmh="best")
    
    ##################################################################
    if (verbose) message("Finally, append the response data (hits and mqe) into the sMap object...", appendLF=T)
    
    ## for hits
    hits <- sapply(seq(1,sM_final$nHex), function(x) sum(response$bmh==x))
    
    sMap <- list(  nHex = sM_final$nHex, 
                   xdim = sM_final$xdim, 
                   ydim = sM_final$ydim,
                   lattice = sM_final$lattice,
                   shape = sM_final$shape,
                   coord = sM_final$coord,
                   init = sM_final$init,
                   neighKernel = sM_final$neighKernel,
                   codebook = sM_final$codebook,
                   hits = hits,
                   mqe = response$mqe,
                   call = match.call(),
                   method = "suprahex")
                   
    class(sMap) <- "sMap"
    
    if(verbose){
    
        message("", appendLF=T)
    
        message("Below are the summaries of the training results:", appendLF=T)
        summary <- vector()
        summary[1] <- paste(c("   dimension of input data: ", dim(data)[1], "x", dim(data)[2], "\n"), collapse="")
        summary[2] <- paste(c("   xy-dimension of map grid: ", "xdim=", sMap$xdim, ", ydim=", sMap$ydim, "\n"), collapse="")
        summary[3] <- paste(c("   grid lattice: ", sMap$lattice, "\n"), collapse="")
        summary[4] <- paste(c("   grid shape: ", sMap$shape, "\n"), collapse="")
        summary[5] <- paste(c("   dimension of grid coord: ", dim(sMap$coord)[1], "x", dim(sMap$coord)[2], "\n"), collapse="")
        summary[6] <- paste(c("   initialisation method: ", sMap$init, "\n"), collapse="")
        summary[7] <- paste(c("   dimension of codebook matrix: ", dim(sMap$codebook)[1], "x", dim(sMap$codebook)[2], "\n"), collapse="")
        summary[8] <- paste(c("   mean quantization error: ", sMap$mqe, "\n"), collapse="")
        message(summary,appendLF=T)
        
        message("Below are the details of trainology:", appendLF=T)
        details <- vector()
        details[1] <- paste(c("   training algorithm: ", algorithm, "\n"), collapse="")
        details[2] <- paste(c("   alpha type: ", alphaType, "\n"), collapse="")
        details[3] <- paste(c("   training neighborhood kernel: ", neighKernel, "\n"), collapse="")
        details[4] <- paste(c("   trainlength (x input data length): ", sT_rough$trainLength," at rough stage; ", sT_finetune$trainLength," at finetune stage", "\n"), collapse="")
        details[5] <- paste(c("   radius (at rough stage): from ", sT_rough$radiusInitial," to ", sT_rough$radiusFinal, "\n"), collapse="")
        details[6] <- paste(c("   radius (at finetune stage): from ", sT_finetune$radiusInitial," to ", sT_finetune$radiusFinal, "\n"), collapse="")
        message(details,appendLF=T)
    }   
    
    ####################################################################################
    endT <- Sys.time()
    message(paste(c("End at ",as.character(endT)), collapse=""), appendLF=T)
    
    runTime <- as.numeric(difftime(strptime(endT, "%Y-%m-%d %H:%M:%S"), strptime(startT, "%Y-%m-%d %H:%M:%S"), units="secs"))
    message(paste(c("Runtime in total is: ",runTime," secs\n"), collapse=""), appendLF=T)
    
    invisible(sMap)
}