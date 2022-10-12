to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}


alpha <- function (colour, alpha = NA) {
  col <- col2rgb(colour, TRUE)/255
  if (length(colour) != length(alpha)) {
    if (length(colour) > 1 && length(alpha) > 1) {
      stop("Only one of colour and alpha can be vectorised")
    }
    if (length(colour) > 1) {
      alpha <- rep(alpha, length.out = length(colour))
    }
    else if (length(alpha) > 1) {
      col <- col[, rep(1, length(alpha)), drop = FALSE]
    }
  }
  alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
  new_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
  new_col[is.na(colour)] <- NA
  new_col
}


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


# Readmet <- function(ring){
#   fn <- sprintf("Rings/Ring%s/runfolder/met.dat",ring)
#   met <- read.table(fn,head=FALSE, skip=24)
#   attach(met)
#   names(met)<-c("CA","PPT","PAR","TAIR","RH")
#   return(met)
# }

ReadInput <- function(ring){
  # fn <- sprintf("Rings/Ring%s/runfolder/met_ListOfAllVaule.csv",1)
  # InputValue <- read.csv(fn)
  # ring=1

  con.path <- file.path(sprintf("Rings/Ring%s/runfolder",ring),"confile.dat")
  con.ls <- readLines(con.path)

  # substr(con.ls[grep("startdate", con.ls)],
  #        "startdate = ","")

 sd <- gsub("startdate = ","",
     con.ls[grep("startdate", con.ls)])
 sd <- gsub("'","",sd)

 s.date <- as.Date(sd,"%d/%m/%y")
 
 ed <- gsub("enddate = ","",
            con.ls[grep("enddate", con.ls)])
 ed <- gsub("'","",ed)
 
 e.date <- as.Date(ed,"%d/%m/%y")
 
 fn <- sprintf("Rings/Ring%s/runfolder/met.dat",ring)
 
 # met <- read.table(fn,head=FALSE, skip=24)

 # names(met)<-c("CA","PRESS","WIND",
 #               "PPT","PAR","TAIR","RH")
 
 met <- readmet(fn)
# met.date.vec <-  rep(seq(s.date,e.date,by="1 day"),each=48)
 met$Date <- rep(seq(s.date,e.date,by="1 day"),each=48, length.out=nrow(met))
 
return(met)
}

ReadDayFlux <- function(ring){
  fn <- sprintf("Rings/Ring%s/runfolder/Dayflx.dat",ring)
  # DayFlux <- read.table(fn,head=FALSE, skip=22)
  # # attach(DayFlux)
  # names(DayFlux)<-c("DOY", "Tree", "Spec", "absPAR", "absNIR", "absTherm",
  #                   "totPs", "totRf", "netPs", "totLE1", "totLE2","totH")
  
  DayFlux <- readdayflux(fn)
  
  return(DayFlux)
}

ReadHourFlux <- function(ring){
  fn <- sprintf("Rings/Ring%s/runfolder/hrflux.dat",ring)
  # DayFlux <- read.table(fn,head=FALSE, skip=35)
  # # attach(DayFlux)
  # names(DayFlux)<-c("DOY", "Tree", "Spec", "absPAR", "absNIR", "absTherm",
  #                   "totPs", "totRf", "netPs", "totLE1", "totLE2","totH")
  HourFlux <-readhrflux(filename = fn)
  return(HourFlux)
}

GetAverage <- function (x) {
  attach(x)

  y <- data.table(x)[,list(CA=mean(CA, na.rm=TRUE),
                           PAR = 1800*sum(PAR, na.rm=TRUE)*10^-6/4.57,
                           RH = mean(RH, na.rm=TRUE),
                           TAIR = mean(TAIR, na.rm=TRUE),
                           Rain = sum(PPT, na.rm=TRUE)),
                     by = Date]
  return(y) 
}

getAllRings<-function(DayFlux,InputValue){
  ##Data process#####
  DailyAverage.flux <- list()
  DailyAverage.input <- list()
  DailyAverage.Ring <- list()
  for (i in 1:6){
    DailyAverage.flux[[i]] <- summaryBy(totPs + totRf + absPAR + totLE1 ~ DOY,
                                        data = DayFlux[[i]],
                                        FUN = sum,na.rm = TRUE)
    names(DailyAverage.flux[[i]]) <- c("DOY","GPP","Ra","absPAR","le")
    
    # DailyAverage.flux[[i]]$soil.e <- watbal.d[[i]]$soilevap
    
    DailyAverage.input[[i]] <- GetAverage(InputValue[[i]])
    DailyAverage.input[[i]]$Ring <- i
    DailyAverage.input[[i]]$DOY <- c(1:nrow(DailyAverage.input[[i]])) 
    DailyAverage.input[[i]] <- subset(DailyAverage.input[[i]],DOY <= nrow(DailyAverage.flux[[i]]))
    
    
    DailyAverage.Ring[[i]] <- merge(DailyAverage.flux[[i]] ,DailyAverage.input[[i]],by="DOY")
    
  }
  
#   #merge data sets
  AllRings <- do.call("rbind", DailyAverage.Ring)
  return(AllRings)
}

getVPD <- function(RH,TAIR){
  
  VPD <- (1-RH)*0.61375*exp(17.502*TAIR/(240.97+TAIR))
  
  return(VPD)
}

getAllData <- function(...){
  
  #the data in AllData is used for finally analysis
  #only GPP is from Dayflx all others from InputValue which is read from met with date and missing value
  
  #Data import######################
  #get intput(met and inputvalue) *inputvalue has the accurate dates but also the missing value
  
  #data process
  AllRings <- getAllRings(...)
  AllRings$Date <- as.Date(AllRings$Date)
  #get VPD
  AllRings$VPD <- getVPD(AllRings$RH,AllRings$TAIR)
  #incorperate LAI
  for(i in 1:6){
    
    AllRings$LAI[AllRings$Ring == i] <-sm[[i]]$LAIsmooth 
    AllRings$dL[AllRings$Ring == i] <-sm[[i]]$dLAI
  }
  
  #replace missing Value
  AllRings$CA <- na.locf(AllRings$CA)
  AllRings$PAR <- na.locf(AllRings$PAR)
  AllRings$RH <- na.locf(AllRings$RH)
  AllRings$TAIR <- na.locf(AllRings$TAIR)
  AllRings$VPD <- na.locf(AllRings$VPD)
  
  
  #get GOL and all
  AllRings$GOL <- AllRings$GPP/AllRings$LAI
  AllRings$Ring <-as.character(AllRings$Ring)
  AllRings$dL[is.na(AllRings$dL) == TRUE] <-0
  return(AllRings)
}


# 

get.hr.Rings <- function(DayFlux){
  
  ##Data process#####
  ring.Average.flux <- list()
  
  for (i in 1:6){
    ring.Average.flux[[i]] <- summaryBy(hrPs + hrLE + VPD + PAR + TAIR~ 
                                          DOY + HOUR,
                                        data = DayFlux[[i]],
                                        FUN = sum,na.rm = TRUE)
    
    names(ring.Average.flux[[i]]) <- c("DOY","HOUR","GPP","ET","VPD","PAR","TAIR")
    
    ring.Average.flux[[i]]$Ring <- i
    repli.num <- length(DayFlux[[i]]$TAIR[which(DayFlux[[i]]$DOY == DayFlux[[i]]$DOY[[1]] &
                                                  DayFlux[[i]]$HOUR == DayFlux[[i]]$HOUR[[1]])])
    ring.Average.flux[[i]]$TAIR <- ring.Average.flux[[i]]$TAIR / repli.num
    con.path <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
    test <- readLines(con.path)
    date.start <- str_sub(test[grep("startdate",test)],-9,-2)
    date.start <- as.Date(date.start,"%d/%m/%y")
    date.end <- str_sub(test[grep("enddate",test)],-9,-2)
    date.end <- as.Date(date.end,"%d/%m/%y")
    
    
    tree.tar <- str_sub(test[grep("itargets",test)],12,-1)
    
    tree.num <- length(strsplit(tree.tar," ")[[1]])
    
    ring.Average.flux[[i]]$VPD <- ring.Average.flux[[i]]$VPD / tree.num
    
    dates <- seq(to=date.end,from = date.start,"day")
    
    ring.Average.flux[[i]]$Date <- dates[ring.Average.flux[[i]]$DOY]
    
    ring.Average.flux[[i]]$LAI <- sm[[i]]$LAIsmooth[ring.Average.flux[[i]]$DOY]
  }
  
  # ring.Average.flux$Date <- seq(range(Date.in),)
  AllRings <- do.call("rbind",ring.Average.flux)
  
  return(AllRings)
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column 
  library(reshape)
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

get_soilwater <- function(how=c("mean","byring")){
  
  how <- match.arg(how)
  
  meanVWC <- function(dfr){
    vwccols <- grep("VWC_",names(dfr))
    dfr <- dfr[,vwccols]
    dfr[dfr > 1] <- 1
    rowMeans(dfr, na.rm=FALSE)
  }
  
  soilw <- downloadTOA5("SoilVars", maxnfiles=500)
  soilw$Ring <- as.factor(paste0("R",  str_extract(soilw$Source, "[0-9]")))
  
  if(how == "mean"){
    soilwd <- summaryBy(. ~ Date, FUN=mean, keep.names=TRUE, data=soilw, na.rm=TRUE)
    soilwd <- data.frame(Date=soilwd[,c("Date")], VWC=meanVWC(soilwd))
  } 
  if(how == "byring"){
    
    soilwd <- summaryBy(. ~ Ring + Date, FUN=mean, keep.names=TRUE, data=soilw, na.rm=TRUE)
    soilwd <- data.frame(Date=soilwd$Date, Ring=soilwd$Ring, VWC=meanVWC(soilwd))
  }
  
  return(soilwd)
}

# test <- readLines("confile.dat")

