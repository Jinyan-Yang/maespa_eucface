source('r/get spots g1.r')
#######
# get 1st points of aci curves
source('r/clean aci.r')

swc.neutron.ring.df <- readRDS('cache/swc.rds')

euc.amb.df <- euc.all.df#[euc.all.df$Ring %in% c(2,3,6),]

euc.1st.df <- euc.amb.df[firstobs(~Number,data=euc.all.df),] 
euc.1st.df <- euc.1st.df[!is.na(euc.1st.df$Number),]
euc.1st.df <- euc.1st.df[euc.1st.df$CO2R < 450 & euc.1st.df$CO2R > 370,]
euc.1st.df$Campaign <- droplevels(euc.1st.df$Campaign)
euc.1st.df$group <- (paste(euc.1st.df$Campaign,euc.1st.df$Ring))

temp.ls <- split(euc.1st.df,euc.1st.df$group)

for (i in 1:length(temp.ls)){
  if(nrow(temp.ls[[i]]) < 2){
    temp.ls[[i]] <- NA
  }
}

euc.long.df <- do.call(rbind,temp.ls)
euc.long.df <- euc.long.df[!is.na(euc.long.df$Number),]
library(plantecophys)
fit.first <- fitBBs(euc.long.df,"group",gsmodel = c("BBOpti"))
first.g1 <- merge(coef(fit.first),euc.long.df,by.x="group",by.y="group")
first.g1 <- first.g1[order(first.g1$Number),]
first.g1 <- first.g1[first.g1$Date > as.Date('2013-01-01'),]
first.g1$Campaign <- droplevels(first.g1$Campaign)
first.g1$Campaign <- factor(c('ACi'))

first.g1 <- first.g1[,c('Date', 'Campaign', 'Ring','g1')]
first.g1$Ring <- paste0('R',first.g1$Ring)
first.g1 <- first.g1[!duplicated(first.g1),]
# all.g1.euc.df <- merge(spots.g1.one,first.g1,all=TRUE)
all.g1.euc.df <- spots.g1.one
g1.swc.ls <- list()
for(i in 1:6){
  g1.ring.df <- all.g1.euc.df[all.g1.euc.df$Ring == paste0("R",i),]
  
  swc.ring.df <- swc.neutron.ring.df[swc.neutron.ring.df$Ring == paste0("R",i),]
  
  r.out.ls <- list()
  
  for(j in 1:length(unique(g1.ring.df$Date))){
    diff.day <- swc.ring.df$Date - unique(g1.ring.df$Date)[j]
    min.diff <- min(abs(diff.day),na.rm=TRUE)
    if(min.diff < 14){
      sele.index <- which(abs(diff.day) == min.diff)[1]
    }else{
      sele.index <- NA
    }
    
    if(length(sele.index) > 0){
      r.out.ls[[j]] <- swc.ring.df[sele.index,]
      r.out.ls[[j]]$g1 <- g1.ring.df$g1[j]
      r.out.ls[[j]]$Campaign <- g1.ring.df$Campaign[j]
    }else{
      r.out.ls[[j]] <- NULL
    }
  }
  g1.swc.ls[[i]] <- do.call(rbind,r.out.ls)
}

g1.swc.df <- do.call(rbind,g1.swc.ls)
g1.swc.df <- g1.swc.df[complete.cases(g1.swc.df),]
g1.swc.df$swc.100 <- g1.swc.df$vwc.neu / 100
# 

if(!file.exists('cache/ros.gs.rds')){
  source("r/get ros g1 swc.r")
}else{
  eute.ros <- readRDS('cache/ros.gs.rds')
}
# 
eute.ros$g1.norm <- (eute.ros$g1 - min(eute.ros$g1)) / (max(eute.ros$g1) - min(eute.ros$g1))

g1.swc.df$g1.norm <- (g1.swc.df$g1 - min(g1.swc.df$g1)) / 
  (max(g1.swc.df$g1) - min(g1.swc.df$g1))

# # fitting
startlist = list(swc.max=0.5,swc.min=0 ,q = 1)

test.df <- data.frame(g1 = c(eute.ros$g1,g1.swc.df$g1),
                      swc = c(eute.ros$TDR,g1.swc.df$swc.100))

test.df <- test.df[test.df$g1 > 0,]
test.df <- test.df[complete.cases(test.df),]


fit.nl.tdr <- nls(g1 ~ 4.3*((swc - swc.min) / (swc.max - swc.min))^q, 
                  data = test.df,
                  start=startlist,algorithm="port",
                  lower=c(0.1,0,0.1),upper=c(2,0.1,3))

test.df$g1.pred <- predict(fit.nl.tdr)
test.df$g1.actual.pred <- test.df$g1.pred * (max(test.df$g1)) 

test.df$vwc <- test.df$swc*100
# library(ochRe)
# col.oz <- ochre_palettes$olsen_qual[7:2]
# # palette(tran.func(col.oz))
# palette(col.oz)
par(mar=c(5,5,1,1))
plot(g1~vwc.neu,data = g1.swc.df,pch=16,col='coral',xlim=c(0,50),ylim=c(0,6),
     xlab=expression(theta~('%')),ylab=expression(g[1]~(kPa^0.5)))
points(g1~c(100*TDR),data =eute.ros,pch=16,col="lightskyblue")
lines(g1.pred ~ vwc,data=test.df[order(test.df$swc),],col="blueviolet",lwd=2)
theta.vec <- seq(0,0.35,0.001)
g1.drake <- (theta.vec/0.6)^0.32*(max(test.df$g1) - min(test.df$g1)) + min(test.df$g1)
lines(g1.drake ~ c(theta.vec*100),
      col="lightskyblue",lwd=2,lty='dashed')
# legend("topleft",legend = c(paste0("xmax = ",format(coef(fit.nl.tdr)[[1]],digits = 2)),
#                             "xmin = 0",
#                             paste0("q = ",format(coef(fit.nl.tdr)[[3]],digits = 2))))
legend("bottomright",legend = c("Drake et al., 2017","EucFACE"),
       pch=c(16),bty='n',
       col=c('lightskyblue','coral'))

saveRDS(g1.swc.df,"cache/g1.swc.df.rds")

saveRDS(fit.nl.tdr,"cache/fit.g1.swc.rds")

saveRDS(test.df,"cache/g1.euc.ros.rds")


# g1.swc.df <- readRDS("cache/g1.swc.df.rds")

# swc.neutron.ring.df <- readRDS('cache/swc.rds')
# # psl.e = -0.35 # MPA; from Duursma 2015
# # swc.s = 0.6 #m3 m-3
# # b = 4.3 #
# # swc.neutron.ring.df$S.PSI <- psl.e*(swc.neutron.ring.df$VWC/100/swc.s)^-b/1000
# 
# # get g1####
# fit.g1.func <- function(spots,byRing = TRUE){
#   # spots <- read.csv("data/Gimeno_spot_Eter_gasExchange6400.csv")
#   spots <- spots[is.na(spots$Tree) != TRUE,]
#   spots$Date <- as.character(spots$Date)
#   spots$Date[spots$Date == "31/11/2012"] <- "31/10/2012"
#   spots$Date <- as.Date(as.character(spots$Date),"%d/%m/%Y")
#   spots$Date[spots$Date > as.Date("2013-09-08") & spots$Date < as.Date("2013-09-15")] <- as.Date("2013-09-10")
#   spots$Campaign <- droplevels(spots$Campaign)
# 
#   if(byRing==TRUE){
#   fit.spot <- fitBBs(spots,"group",gsmodel = c("BBOpti"))
#   spots.g1 <- merge(coef(fit.spot),unique(spots[,c("Ring","Date","group","Campaign","Measurement")]),
#                     by.x="group",by.y="group",all.y=FALSE)
#   spots.g1 <- spots.g1[complete.cases(spots.g1),]
#   spots.g1 <- spots.g1[order(spots.g1$Date),]
#   }else{
#     spots$Campaign <- as.character(spots$Campaign)
#     fit.spot <- fitBBs(spots,'Campaign',gsmodel = c("BBOpti"))
#     spots.g1 <- merge(coef(fit.spot),unique(spots[,c("Date","Campaign")]),
#                       by.x="group",by.y="Campaign",all.y=FALSE)
#     spots.g1 <- spots.g1[complete.cases(spots.g1),]
#     spots.g1 <- spots.g1[order(spots.g1$Date),]
#   }
#   return(spots.g1)
# }
# 
# spots <- read.csv(unz("download/Gimeno_spot.zip",
#                       "data/Gimeno_spot_Eter_gasExchange6400.csv"))
# spots <- spots[complete.cases(spots$Photo),]
# # remove pred treat data which doesn't has swc with them anyway
# spots <- spots[-(grep("2012Apr",as.character(spots$Campaign))),]
# spots <- spots[-(grep("2012May",as.character(spots$Campaign))),]
# spots$group <- paste0(as.character(spots$Campaign),"-",as.character(spots$Ring),"+",spots$Measurement)
# # spots$facotrs <- paste0(spots$Campaign,"+",spots$Measurement)
# # spots <- spots[spots$group != "-",]
# spots.g1.ma <- fit.g1.func(spots)
# 
# spots.g1.ma$Ring <- as.character(spots.g1.ma$Ring)
# spots.g1.ma <- tidyr::spread(spots.g1.ma,Measurement,g1)
# spots.g1.ma <- doBy::summaryBy(Anet_aft + Anet_mor ~ Date + Campaign + Ring,
#                                data = spots.g1.ma,keep.names = TRUE,FUN=mean,na.rm=TRUE)
# names(spots.g1.ma) <- c("Date","Campaign","Ring","g1.aft","g1.mor")
# 
# spots$group <- paste0(as.character(spots$Campaign),"-",as.character(spots$Ring))
# spots.g1.one <- fit.g1.func(spots)
# spots.g1.one <- doBy::summaryBy(g1 ~ Date + Campaign + Ring,
#                                 data = spots.g1.one,keep.names = TRUE,FUN=mean,na.rm=TRUE)
# spots.g1 <- merge(spots.g1.ma,spots.g1.one,by=c("Date","Campaign","Ring"))
# spots.g1$Date[spots.g1$Campaign == "2013Nov"] <- as.Date("2013-11-07")
# 
# # 
# spots.by.campaign <- fit.g1.func(spots,byRing = FALSE)
# spots.by.campaign <- summaryBy(.~group,data = spots.by.campaign,
#                                FUN=mean,keep.names = T,id=~Date)
# # get tdr swc####
# if(file.exists('cache/swc.day.df.rds')){
#   swc.day.df <- readRDS('cache/swc.day.df.rds')
# }else{
#   source('r/process TDR.r')
# }
# 
# 
# # get leaf wp####
# leaf.wp.df <- downloadCSV("Gimeno_spots_Eter_EucFACE_waterPotential.csv")
# library(doBy)
# leaf.wp.df <- summaryBy(WP~Campaign+Ring+Measurement,
#                         data = leaf.wp.df,FUN=mean,na.rm=TRUE,keep.names = TRUE,id=~CO2_Treat2)
# leaf.wp.df.long<- tidyr::spread(leaf.wp.df, "Measurement","WP")
# 
# spots.g1.wp <- merge(spots.g1,leaf.wp.df.long,by=c("Campaign","Ring"),all.x=TRUE)
# 
# leaf.wp.by.campaign <- summaryBy(Midday + Morning + Predawn~Campaign,
#                                  data = leaf.wp.df.long,FUN=mean,na.rm=TRUE,keep.names = TRUE)
# 
# spots.by.campaign.wp <- merge(spots.by.campaign,leaf.wp.by.campaign,by.y=c("Campaign"),by.x='group',all.x=TRUE)
# 
# # leaf.wp.df$Campaign <- as.factor(leaf.wp.df$Campaign)
# # plot(WP~Campaign,data = leaf.wp.df[leaf.wp.df$Ring == "R1" & leaf.wp.df$Measurement == "Predawn",])
# 
# # put thing together####
# g1.swc.ls <- list()
# for(i in 1:6){
#   g1.ring.df <- spots.g1.wp[spots.g1.wp$Ring == paste0("R",i),]
#   
#   swc.ring.df <- swc.neutron.ring.df[swc.neutron.ring.df$Ring == paste0("R",i),]
#   
#   r.out.ls <- list()
# 
#     for(j in 1:length(unique(g1.ring.df$Date))){
#       diff.day <- swc.ring.df$Date - unique(g1.ring.df$Date)[j]
#       min.diff <- min(abs(diff.day),na.rm=TRUE)
#       if(min.diff < 14){
#         sele.index <- which(abs(diff.day) == min.diff)[1]
#       }else{
#         sele.index <- NA
#       }
#       
#       if(length(sele.index) > 0){
#         r.out.ls[[j]] <- swc.ring.df[sele.index,]
#         r.out.ls[[j]]$g1 <- g1.ring.df$g1[j]
#         r.out.ls[[j]]$Campaign <- g1.ring.df$Campaign[j]
#       }else{
#         r.out.ls[[j]] <- NULL
#       }
#     }
#     g1.swc.ls[[i]] <- do.call(rbind,r.out.ls)
# }
# 
# # 
# see <- merge(spots.g1.wp,do.call(rbind,g1.swc.ls),by=c("Campaign","Ring","g1"),all=TRUE)
# names(see)[names(see) == 'Date.x'] <- "Date"
# names(see)[names(see) == 'Date.y'] <- "Date.neutron"
# g1.swc.df <- merge(see,swc.day.df,by =c("Date","Ring"))
# g1.swc.df$swc.0.30.tdr <- (g1.swc.df$swc.tdr.5 + g1.swc.df$swc.tdr.30)/2
# g1.swc.df <- g1.swc.df[!duplicated(g1.swc.df),]
# g1.swc.df$Campaign <- droplevels(g1.swc.df$Campaign)
# g1.swc.df$swc.100 <- g1.swc.df$vwc.300 / 100
# 
# neutron.by.campaign <- summaryBy(swc.100 + vwc.300~Campaign,data = g1.swc.df,
#                                  FUN=mean,na.rm=TRUE,keep.names = TRUE)
# 
# g1.wp.swc.df.by.campaign <- merge(spots.by.campaign.wp,neutron.by.campaign,by.x='group',by.y='Campaign')
# g1.wp.swc.df.by.campaign$group <- droplevels(g1.wp.swc.df.by.campaign$group)
# par(mfrow=c(2,1),mar=c(5,5,1,1))
# library(ochRe)
# col.oz <- ochre_palettes$olsen_qual[6:2]
# palette(col.oz)
# plot(Predawn~vwc.300,data = g1.wp.swc.df.by.campaign,pch=16,col=group)
# # plot(Predawn~vwc.400,data = g1.wp.swc.df.by.campaign,pch=16,col=group)
# 
# legend("bottomright",legend = c(levels(g1.wp.swc.df.by.campaign$group)),
#        col=c(palette()),pch=c(16,16,16,16),ncol = 1)
# # #####
# g1.swc.df.sub <- g1.swc.df#[g1.swc.df$Campaign != "2013Nov",]
# if(!file.exists('cache/ros.gs.rds')){
#   source("r/get ros g1 swc.r")
# }else{
#   eute.ros <- readRDS('cache/ros.gs.rds')
# }
# 
# # 
# eute.ros$g1.norm <- eute.ros$g1 / (max(eute.ros$g1))
# 
# # fit.nl.ros <- nls(g1.norm~((TDR - swc.min) / (swc.max - swc.min))^q, 
# #                   data = eute.ros,start=startlist,algorithm="port",
# #                   lower=c(0.1,0,0.01),upper=c(0.6,0.0001,3))
# 
# eute.ros$g1.norm <- (eute.ros$g1 - min(eute.ros$g1)) / (max(eute.ros$g1) - min(eute.ros$g1))
# g1.swc.df.sub$g1.norm <- (g1.swc.df.sub$g1 - min(g1.swc.df.sub$g1)) / 
#   (max(g1.swc.df.sub$g1) - min(g1.swc.df.sub$g1))
# 
# # # fitting
# startlist = list(swc.max=0.5,swc.min=0 ,q = 1)
# # 
# # fit.nl.ros <- nls(g1.norm~((TDR - swc.min) / (swc.max - swc.min))^q, 
# #                      data = eute.ros,start=startlist,algorithm="port",
# #                   lower=c(0.1,0,0.01),upper=c(0.6,0.0001,3))
# 
# # fit.nl.g1norm <- nls(g1.norm~((swc.100 - swc.min) / (swc.max - swc.min))^q, 
# #                      data = g1.swc.df.sub,
# #                      start=startlist,algorithm="port",
# #                      lower=c(0.1,0,0.01),upper=c(0.6,0.0001,3))
# 
# test.df <- data.frame(g1 = c(eute.ros$g1,g1.swc.df.sub$g1),
#                       swc = c(eute.ros$TDR,g1.swc.df.sub$swc.100))
# 
# # test.df <- data.frame(g1 = c(eute.ros$g1,g1.swc.df.sub$g1),
# #                       swc = c(eute.ros$TDR,g1.swc.df.sub$swc.tdr/100))
# 
# # test.df <- data.frame(g1 = c(g1.swc.df.sub$g1),
# #                       swc = c(g1.swc.df.sub$swc.tdr/100))
# test.df <- test.df[test.df$g1 > 0,]
# test.df <- test.df[complete.cases(test.df),]
# # summary(lm(log(test.df$g1)~log(test.df$swc)))
# 
# # test.df$g1.norm <- (test.df$g1 - min(test.df$g1)) / 
# #   (max(test.df$g1) - min(test.df$g1))
# # plot(g1~swc,data = test.df)
# # test.df$g1.norm <- (test.df$g1 ) / (max(test.df$g1) )
# # summary(lm(log(g1)~log(swc),data = test.df))
# # fit.nl.both <- nls(g1.norm~((swc - swc.min) / (swc.max - swc.min))^q, 
# #                      data = test.df,
# #                      start=startlist,algorithm="port",
# #                      lower=c(0.1,0,0.1),upper=c(2,0,3))
# 
# fit.nl.tdr <- nls(g1 ~ 4.3*((swc - swc.min) / (swc.max - swc.min))^q, 
#                    data = test.df,
#                    start=startlist,algorithm="port",
#                    lower=c(0.1,0,0.1),upper=c(2,0.1,3))
# # summary(fit.nl.tdr)
# 
# test.df$g1.pred <- predict(fit.nl.tdr)
# test.df$g1.actual.pred <- test.df$g1.pred * (max(test.df$g1)) 
# 
# # saveRDS(fit.nl.tdr,"cache/fit.g1.swc.rds")
# # 
# # saveRDS(test.df,"cache/g1.euc.ros.rds")
# # saveRDS(g1.swc.df,"cache/g1.swc.df.rds")
# # saveRDS(eute.ros,"cache/ros.g1.swc.df.rds")
# # 
# # fit.nl.tdr <- readRDS("cache/fit.g1.swc.rds")
# # test.df <- readRDS("cache/g1.euc.ros.rds")
# # g1.swc.df <- readRDS("cache/g1.swc.df.rds")
# # eute.ros <- readRDS("cache/ros.g1.swc.df.rds")
# 
# # plot(log(g1)~log(swc),data = test.df)
# 
# # test.df.tdr <- data.frame(g1 = c(eute.ros$g1,g1.swc.df.sub$g1),
# #                           swc = c(eute.ros$TDR,g1.swc.df.sub$swc.tdr/100))
# 
# par(mfrow=c(1,1))
# par(mar=c(5,5,1,1))
# plot(g1~swc.100,data = g1.swc.df.sub,pch=16,col=Campaign,xlim=c(0,0.5),ylim=c(0,6),
#      xlab=expression(theta~("%")),ylab=expression(g[1]~(kPa^0.5)))
# points(g1~(TDR),data =eute.ros,pch=16,col="grey")
# lines(g1.pred ~ swc,data=test.df[order(test.df$swc),],col="black",lwd=2)
# theta.vec <- seq(0,0.5,0.001)
# g1.drake <- (theta.vec/0.6)^0.32*(max(test.df$g1) - min(test.df$g1)) + min(test.df$g1)
# lines(g1.drake ~ theta.vec,
#       col="black",lwd=2,lty='dashed')
# # legend("topleft",legend = c(paste0("xmax = ",format(coef(fit.nl.tdr)[[1]],digits = 2)),
# #                             "xmin = 0",
# #                             paste0("q = ",format(coef(fit.nl.tdr)[[3]],digits = 2))))
# legend("bottomright",legend = c("Drake et al., 2017a","All"),lty=c("dashed","solid",lwd=2),bty='n')
# legend("topleft",legend = c(levels(g1.swc.df.sub$Campaign)),
#        col=palette(),pch=c(16,16,16,16),ncol = 2,bty='n')
# 
# stop('nothing')
# # plot
# pdf("g1~swc.pdf",width = 8,height = 8)
# par(mfrow=c(2,2),mar=c(5,5,1,1))
# # palette(rainbow(length(levels(g1.swc.df$Campaign))))
# library(ochRe)
# col.oz <- ochre_palettes$olsen_qual[6:2]
# # palette(tran.func(col.oz))
# palette(col.oz)
# plot(g1~c(swc.100),data = g1.swc.df,pch=16,col=Campaign,xlim=c(0,0.35))
# # # legend("bottomright",legend = c(levels(g1.swc.df$Campaign)),pch=16,col=c(palette()),bty='n')
# # legend("bottomright",legend = c(levels(g1.swc.df$Campaign),"ROS"),pch=16,col=c(palette(),"grey"),bty='n')
# # plot(g1~swc.100,data = g1.swc.df,pch=16,col=Campaign,xlim=c(0,0.35))
# # points(g1~(TDR),data =eute.ros,pch=16,col="grey")
# # legend("bottomright",legend = c(levels(g1.swc.df$Campaign),"ROS"),pch=16,col=c(palette(),"grey"),bty='n')
# par(mar=c(5,5,1,1))
# plot(g1~swc.100,data = g1.swc.df.sub,pch=16,col=Campaign,xlim=c(0,0.35),ylim=c(0,6),
#      xlab=expression(theta~('%')),ylab=expression(g[1]~(kPa^0.5)))
# points(g1~(TDR),data =eute.ros,pch=16,col="grey")
# lines(g1.pred ~ swc,data=test.df[order(test.df$swc),],col="black",lwd=2)
# theta.vec <- seq(0,0.35,0.001)
# g1.drake <- (theta.vec/0.6)^0.32*(max(test.df$g1) - min(test.df$g1)) + min(test.df$g1)
# lines(g1.drake ~ theta.vec,
#       col="black",lwd=2,lty='dashed')
# # legend("topleft",legend = c(paste0("xmax = ",format(coef(fit.nl.tdr)[[1]],digits = 2)),
# #                             "xmin = 0",
# #                             paste0("q = ",format(coef(fit.nl.tdr)[[3]],digits = 2))))
# legend("bottomright",legend = c("Drake et al., 2017a","All"),lty=c("dashed","solid",lwd=2),bty='n')
# legend("topleft",legend = c(levels(g1.swc.df.sub$Campaign)),
#        col=palette(),pch=c(16,16,16,16),ncol = 2,bty='n')
# 
# # plot(g1.norm ~ g1.pred,data=test.df,pch=16,xlim=c(0,1),ylim=c(0,1))
# # abline(a=0,b=1)
# # fit.result <- summary(lm(g1.norm ~ g1.pred,data=test.df))
# # legend("topleft",legend = paste0("R2 = ",format(fit.result$r.squared,digits = 2)))
# dev.off()
# # g1.nl.pred <- predict(fit.nl.g1norm)
# # plot(g1.swc.df.sub$g1.norm ~ g1.nl.pred,pch=16)
# # 
# # abline(a=0,b=1)
# # summary(lm(g1.swc.df.sub$g1.norm ~ g1.nl.pred))
# # 
# # plot(g1.swc.df.sub$VWC ~ g1.nl.pred,pch=16)
# # par(mfrow=c(1,1))
# # plot(g1.norm~VWC ,data = g1.swc.df.sub,pch=16,ylim=c(0,1))
# # points(g1.nl.pred~g1.swc.df.sub$VWC)
# 
# # # plot#####
# # # plot all wp and swc
# # plot(Midday~(swc.0.30.tdr),data = g1.swc.df,pch=16,col="red",ylim=c(-3.5,0))
# # points(Predawn~(swc.0.30.tdr),data = g1.swc.df,pch=16,col="navy")
# # points(Morning~(swc.0.30.tdr),data = g1.swc.df,pch=16,col="coral")
# # 
# # plot(Midday~(VWC),data = g1.swc.df,pch=16,col="red",ylim=c(-3.5,0),
# #      ylab=expression(psi[L]~(MPa)))
# # points(Predawn~(VWC),data = g1.swc.df,pch=16,col="navy")
# # points(Morning~(VWC),data = g1.swc.df,pch=16,col="coral")
# # legend("bottomright",legend = c('Predawn','Morning','Midday'),pch=16,col=c("navy","coral",'red'),bty='n')
# # 
# # plot(swc.0.30.tdr~(VWC),data = g1.swc.df,pch=16,col="red")
# # abline(a=0,b=1)
# # # 
# # par(mfrow=c(2,1))
# # palette(c("navy","red"))
# # plot(g1~(swc.0.30.tdr),data = g1.swc.df,pch=16,col=CO2)
# # legend("bottomright",legend = levels(g1.swc.df$CO2),pch=16,col=palette(),bty='n')
# # plot(g1~(VWC),data = g1.swc.df,pch=16,col=CO2)
# # legend("bottomright",legend = levels(g1.swc.df$CO2),pch=16,col=palette(),bty='n')
# # # legend("top",legend = levels(g1.swc.df$CO2),pch=16,col=palette(),bty='n',horiz = TRUE,xpd = TRUE,inset = -0.05)
# # # fit.all.psi.nls <- nls(g1~a*exp(b*S.PSI),data = g1.swc.df,start = list(a=2,b=0.01))
# # # g1 = a exp(b pd)
# # fit.all.nls <- nls(g1~a*exp(b*VWC),data = g1.swc.df,start = list(a=2,b=0.01))
# # fitted.g1 <- coef(fit.all.nls)[[1]]*exp(coef(fit.all.nls)[[2]] * -50:0)
# # g1.pred <- predict(fit.all.nls)
# # summary(lm(g1.pred~g1.swc.df$g1))
# # plot(log(g1)~swc.0.30.tdr,data = g1.swc.df)
# # plot(g1.swc.df$g1~g1.swc.df$S.PSI,pch=16,col="grey")
# # lines(x=-50:0,y=fitted.g1)
# # plot((g1)~Predawn,data = g1.swc.df)
# # summary(lm((g1)~Predawn,data = g1.swc.df))
# # summary(lm((g1)~(VWC),data = g1.swc.df))
# # 
# # df.nov2013 <- g1.swc.df[g1.swc.df$Campaign == "2013Nov",]
# # # 
# # # g1.swc.df$treat <- NA
# # # g1.swc.df$treat[g1.swc.df$Ring %in% paste0("R",c(2,3,6))] <- "A"
# # # g1.swc.df$treat[g1.swc.df$Ring %in% paste0("R",c(1,4,5))] <- "E"
# # # g1.swc.df$treat <- as.factor(g1.swc.df$treat )
# # palette(rainbow(5))
# # plot((g1)~VWC,data = g1.swc.df,col=Campaign,pch=c(16,16))
# # legend("bottomright",legend = levels(g1.swc.df$Campaign),pch=16,col=palette())
# # summary(lm(log(g1)~log(VWC),data = g1.swc.df[g1.swc.df$Campaign != '2013Nov',]))
# # 
# # palette(rainbow(6))
# # g1.swc.df$Ring <- as.factor(g1.swc.df$Ring)
# # plot((g1)~VWC,data = g1.swc.df[g1.swc.df$Campaign == '2013Nov',],col=Ring,pch=c(16,16))
# # legend("bottomleft",legend = levels(g1.swc.df$Ring),pch=16,col=palette())
# # 
# # palette(rainbow(5))
# # plot((g1.aft)~VWC,data = g1.swc.df,col=Campaign,pch=c(16,16),ylim=c(2,7))
# # points((g1.aft)~VWC,data = g1.swc.df,col=Campaign,pch=3)
# # points((g1.mor)~VWC,data = g1.swc.df,col=Campaign,pch=16)
# # legend("bottomright",legend = levels(g1.swc.df$Campaign),pch=16,col=palette())
# # 
# # summary(lm((g1.aft)~VWC,data = g1.swc.df))
# # summary(lm((g1.mor)~VWC,data = g1.swc.df))
# # plot((g1)~Predawn,data = g1.swc.df,col=Campaign,pch=c(16,16))
# # 
# # g1.swc.sum <- doBy::summaryBy(g1 +g1.aft + g1.mor + VWC + S.PSI + swc.0.30.tdr + Predawn + Midday + Morning ~ Campaign,
# #                               data = g1.swc.df,FUN=median,na.rm=TRUE,keep.names = TRUE)
# # 
# # palette(c("darkseagreen","green","grey","coral","blue"))
# # # plot(g1~(S.PSI),data = g1.swc.sum,pch=16,col="grey")
# # plot(g1~(swc.0.30.tdr),data = g1.swc.sum,pch=16,col=Campaign)
# # plot(g1~(VWC),data = g1.swc.sum,pch=16,col=Campaign)
# # legend("bottomright",legend = levels(g1.swc.df$Campaign),pch=16,col=palette())
# # legend("bottomright",legend = levels(g1.swc.df$Campaign),pch=16,col=palette(),bty='n')
# # 
# # fit.sum.nls <- nls(g1~a*exp(b*VWC),data = g1.swc.sum,start = list(a=2,b=0.01))
# # 
# # fitted.sum.g1 <- coef(fit.sum.nls)[[1]]*exp(coef(fit.sum.nls)[[2]] * g1.swc.sum$VWC)
# # 
# # plot(g1.swc.sum$g1~fitted.sum.g1)
# # lines(x=10:30,y=fitted.g1)
# # 
# # par(mfrow=c(1,1))
# # plot(g1.swc.sum$g1~fitted.sum.g1)
# # abline(a=0,b=1)
# # summary(lm(g1.swc.sum$g1~fitted.sum.g1))
# # 
# # fitted.sum.g1 <- coef(fit.sum.nls)[[1]]*exp(coef(fit.sum.nls)[[2]] * 10:30)
# # plot(g1~VWC,data = g1.swc.sum,pch=16)
# # lines(x=10:30,y=fitted.g1)
# # # par(mfrow=c(2,1))
# # plot(VWC~Date,data = g1.swc.sum,pch=16)
# # plot(WP~Campaign,data = leaf.wp.df[leaf.wp.df$Ring == "R1" & leaf.wp.df$Measurement == "Predawn",])
# # 
# # # #######
# # pdf("g1 water.pdf",width = 8,height = 6)
# # par(mfrow=c(2,1))
# # par(mar=c(1,5,5,1))
# # plot(Midday~(VWC),data = g1.swc.df,pch=16,col="red",ylim=c(-3.5,0),
# #      ylab=expression(psi[L]~(MPa)),xaxt='n')
# # points(Predawn~(VWC),data = g1.swc.df,pch=16,col="navy")
# # points(Morning~(VWC),data = g1.swc.df,pch=16,col="coral")
# # legend("bottomright",legend = c('Predawn','Morning','Midday'),pch=16,col=c("navy","coral",'red'),bty='n')
# # 
# # par(mar=c(5,5,1,1))
# # plot(swc.0.30.tdr~(VWC),data = g1.swc.df,pch=16,col="red")
# # abline(a=0,b=1)
# # 
# # par(mfrow=c(1,3))
# # 
# # palette(c("darkseagreen","green","coral","grey"))
# # # plot(g1~(S.PSI),data = g1.swc.sum,pch=16,col="grey")
# # par(mar=c(5,5,5,1))
# # plot(g1~(swc.0.30.tdr),data = g1.swc.sum,pch=16,col=Campaign)
# # par(mar=c(5,0,5,1))
# # plot(g1~(VWC),data = g1.swc.sum,pch=16,col=Campaign,ylab=' ',yaxt='n')
# # par(mar=c(5,0,5,5))
# # plot(g1~(Predawn),data = g1.swc.sum,pch=16,col=Campaign,ylab=' ',yaxt='n')
# # legend("bottomright",legend = levels(g1.swc.df$Campaign),pch=16,col=palette(),bty='n')
# # dev.off()