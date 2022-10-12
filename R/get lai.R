# # functions####
# to.pdf <- function(expr, filename, ..., verbose=TRUE) {
#   if(!file.exists(dirname(filename)))
#     dir.create(dirname(filename), recursive=TRUE)
#   if ( verbose )
#     cat(sprintf("Creating %s\n", filename))
#   pdf(filename, ...)
#   on.exit(dev.off())
#   eval.parent(substitute(expr))
# }
# 
# 
# alpha <- function (colour, alpha = NA) {
#   col <- col2rgb(colour, TRUE)/255
#   if (length(colour) != length(alpha)) {
#     if (length(colour) > 1 && length(alpha) > 1) {
#       stop("Only one of colour and alpha can be vectorised")
#     }
#     if (length(colour) > 1) {
#       alpha <- rep(alpha, length.out = length(colour))
#     }
#     else if (length(alpha) > 1) {
#       col <- col[, rep(1, length(alpha)), drop = FALSE]
#     }
#   }
#   alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
#   new_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
#   new_col[is.na(colour)] <- NA
#   new_col
# }


makesmoothLAI <- function(dat, timestep="3 days", kgam=15, how=c("byring","mean")){
  
  how <- match.arg(how)
  
  if(how == "mean"){
    
    x <- dat
    x <- x[order(x$Date),]
    gamfit <- smoothplot(as.numeric(Date),LAI,data=x,kgam=kgam, plotit=FALSE)
    
    dfr <- data.frame(X=as.numeric(seq.Date(min(x$Date), max(x$Date), by=timestep)))
    dfr$LAIsmooth <- predict(gamfit[[1]],dfr)
    names(dfr)[1] <- "Date"
    dfr$Date <- as.Date(dfr$Date, origin="1970-1-1")
    
    dfr$dLAI <- c(NA, diff(dfr$LAIsmooth))
    dfr$ndays <- c(NA, diff(dfr$Date))
    return(dfr)
    
  }
  
  if(how == "byring"){
    rings <- split(dat, dat$Ring)
    smoothlai <- lapply(rings, function(x){
      
      x <- x[order(x$Date),]
      
      gamfit <- smoothplot(as.numeric(Date),LAI,data=x,kgam=kgam, plotit=FALSE)
      
      dfr <- data.frame(X=as.numeric(seq.Date(min(x$Date), max(x$Date), by=timestep)))
      dfr$LAIsmooth <- predict(gamfit[[1]],dfr)
      names(dfr)[1] <- "Date"
      dfr$Date <- as.Date(dfr$Date, origin="1970-1-1")
      
      dfr$dLAI <- c(NA, diff(dfr$LAIsmooth))
      dfr$ndays <- c(NA, diff(dfr$Date))
      return(dfr)
    })
  }
  
  return(smoothlai)
}

#' Function for smoothplot. Probably not use otherwise.
fitgam <- function(X,Y,dfr, k=-1, R=NULL){
  dfr$Y <- dfr[,Y]
  dfr$X <- dfr[,X]
  if(!is.null(R)){
    dfr$R <- dfr[,R]
    model <- 2
  } else model <- 1
  dfr <- droplevels(dfr)
  
  
  if(model ==1){
    g <- gam(Y ~ s(X, k=k), data=dfr)
  }
  if(model ==2){
    g <- gamm(Y ~ s(X, k=k), random = list(R=~1), data=dfr)
  }
  
  return(g)
}



#' Plot a generalized additive model
#' @param x Variable for X axis (unquoted)
#' @param y Variable for Y axis (unquoted)
#' @param data Dataframe containing x and y
#' @param kgam the \code{k} parameter for smooth terms in gam.
#' @param R An optional random effect (quoted)
#' @param log Whether to add log axes for x or y (but no transformations are done).
#' @param fitoneline Whether to fit only 
smoothplot <- function(x,y,g=NULL,data,
                       fittype=c("gam","lm"),
                       kgam=4,
                       R=NULL,
                       randommethod=c("lmer","aggregate"),
                       log="",
                       axes=TRUE,
                       fitoneline=FALSE,
                       pointcols=NULL,
                       linecols=NULL, 
                       xlab=NULL, ylab=NULL,
                       polycolor=alpha("lightgrey",0.7),
                       plotit=TRUE, add=FALSE,
                       ...){
  
  fittype <- match.arg(fittype)
  randommethod <- match.arg(randommethod)
  if(log != "")require(magicaxis)
  
  if(!is.null(substitute(g))){
    data$G <- as.factor(eval(substitute(g),data))
  } else {
    fitoneline <- TRUE
    data$G <- 1
  }
  data$X <- eval(substitute(x),data)
  data$Y <- eval(substitute(y),data)
  data <- droplevels(data)
  
  data <- data[!is.na(data$X) & !is.na(data$Y) & !is.na(data$G),]
  nlev <- length(unique(data$G))
  if(length(polycolor) == 1)polycolor <- rep(polycolor,nlev)
  
  if(class(data$X) == "Date"){
    xDate <- TRUE
    data$X <- as.numeric(data$X)
  } else {
    xDate <- FALSE
  }
  
  if(is.null(pointcols))pointcols <- palette()
  if(is.null(linecols))linecols <- palette()
  
  if(is.null(xlab))xlab <- substitute(x)
  if(is.null(ylab))ylab <- substitute(y)
  
  # If randommethod = aggregate, average by group and fit simple gam.
  if(!is.null(R) && randommethod == "aggregate"){
    data$R <- data[,R]
    
    data <- summaryBy(. ~ R, FUN=mean, na.rm=TRUE, keep.names=TRUE, data=data,
                      id=~G)
    R <- NULL
  }
  
  if(!fitoneline){
    
    d <- split(data, data$G)
    
    if(fittype == "gam"){
      fits <- lapply(d, function(x)try(fitgam("X","Y",x, k=kgam, R=R)))
      if(!is.null(R))fits <- lapply(fits, "[[", "gam")
    } else {
      fits <- lapply(d, function(x)lm(Y ~ X, data=x))
    }
    hran <- lapply(d, function(x)range(x$X, na.rm=TRUE))
  } else {
    if(fittype == "gam"){
      fits <- list(fitgam("X","Y",data, k=kgam, R=R))
      if(!is.null(R))fits <- lapply(fits, "[[", "gam")
    } else {
      fits <- list(lm(Y ~ X, data=data))
    }
    hran <- list(range(data$X, na.rm=TRUE))
    
  }
  
  if(plotit){
    if(xDate){
      data$X <- as.Date(data$X, origin="1970-1-1")
    }
    
    if(!add){
      with(data, plot(X, Y, axes=FALSE, pch=16, col=pointcols[G],
                      xlab=xlab, ylab=ylab, ...))
    } else {
      with(data, points(X, Y, pch=16, col=pointcols[G],
                        ...))
    }
    
    if(!add && axes){
      if(log=="xy")magaxis(side=1:2, unlog=1:2)
      if(log=="x"){
        magaxis(side=1, unlog=1)
        axis(2)
        box()
      }
      if(log=="y"){
        magaxis(side=2, unlog=2)
        axis(1)
        box()
      }
      if(log==""){
        if(xDate)
          axis.Date(1, data$X)
        else
          axis(1)
        axis(2)
        box()
      }
    }
    
    for(i in 1:length(fits)){
      
      if(fittype == "gam"){
        nd <- data.frame(X=seq(hran[[i]][1], hran[[i]][2], length=101))
        if(!inherits(fits[[i]], "try-error")){
          p <- predict(fits[[i]],nd,se.fit=TRUE)
          addpoly(nd$X, p$fit-2*p$se.fit, p$fit+2*p$se.fit, col=polycolor[i])
          lines(nd$X, p$fit, col=linecols[i], lwd=2)
        }
      }
      if(fittype == "lm"){
        pval <- summary(fits[[i]])$coefficients[2,4]
        LTY <- if(pval < 0.05)1 else 5
        predline(fits[[i]], col=linecols[i], lwd=2, lty=LTY)
      }
    }
  }
  return(invisible(fits))
}


addpoly <- function(x,y1,y2,col=alpha("lightgrey",0.8),...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=NA,...)
}


# # get gap fraction data from hiev#####
# library(HIEv)
# facelai <- downloadCSV("FACE_P0037_RA_GAPFRACLAI_OPEN_L2.dat")
# names(facelai) <- c("TIMESTAMP","Ring","Date","Gapfraction.mean",
#                     "Rain_mm_Tot.mean","Gapfraction.sd","Rain_mm_Tot.sd",
#                     "Gapfraction.n","Rain_mm_Tot.n","treatment","maxSDring","LAI")
# 
# 
# facelai$Date <- as.Date(facelai$Date)
# 
# # list of smooth LAIs.
# sm <- makesmoothLAI(facelai, how="byring", timestep="1 day")



# get lai from hiev
facelai <-downloadCSV("FACE_P0037_RA_GAPFRACLAI_CORR_2012-10-26_2022-07-05.csv")
names(facelai) <- c('row.no',"Ring","Date","Gapfraction.mean",
                    "Rain_mm_Tot.mean","Gapfraction.sd","Rain_mm_Tot.sd",
                    "Gapfraction.n","Rain_mm_Tot.n","treatment","LAI",'source')

facelai$Date <- as.Date(facelai$Date)

# list of smooth LAIs.
sm <- makesmoothLAI(facelai, how="byring", timestep="1 day",kgam=30)

# test plot with ring 1
r1 <- subset(facelai, Ring== "R1")
with(r1, (plot(Date, LAI)))

s1 <- sm[[1]]
with(s1, (points(Date, LAIsmooth, col = "red")))
