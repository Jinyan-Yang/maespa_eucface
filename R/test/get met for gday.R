# startDate = "2014-01-01"
# endDate = "2014-12-31"

ros15 <- downloadTOA5("ROS_WS_Table15",
                      startDate = startDate,
                      endDate = endDate)
ros05 <- downloadTOA5("ROS_WS_Table05",
                      startDate = startDate,
                      endDate = endDate)


# 
# see <- ros15_30$DateTime[c(ros15_30$DateTime-time.seq)!= 11]

# time.seq <- seq.POSIXt(from = as.POSIXct(startDate),
#                        to = as.POSIXct(strptime(paste0(endDate," 23:30:00"), "%Y-%m-%d %H:%M:%S")),
#                        by="30 min")
# met1.df <- data.frame(DateTime=time.seq)

# fcp <- downloadTOA5(c("FACE","FCPLOGG"), startDate=startDate, endDate=endDate,
#                     maxnfiles=500, tryoffline=TRUE, quiet=TRUE)
# 
# temp1 <- suppressWarnings(thicken(fcp, "30 min", by="DateTime", colname="DateTime2"))
# temp2 <- mutate(temp1,Ring = Plot)
# fcp2 <- subset(temp2[temp2$Ring == Ring,],select=c("DateTime2","WindSpeed","IRGA.Pressure"))
#   
# fcp2.sum <- summaryBy(WindSpeed+IRGA.Pressure~DateTime2,
#                       data=fcp2,FUN=mean,na.rm=TRUE,keep.names=TRUE)
# 
# names(fcp2.sum) <- c("DateTime","WindSpeed", "air_pressure")

ros05_30 <- as.data.frame(dplyr::summarize(group_by(ros05,DateTime=nearestTimeStep(DateTime,30)),
                                           PPFD=mean(PPFD_Avg, na.rm=TRUE),
                                           Tair=mean(AirTC_Avg, na.rm=TRUE),
                                           RH=mean(RH, na.rm=TRUE)))
ros15_30 <- as.data.frame(dplyr::summarize(group_by(ros15,DateTime=nearestTimeStep(DateTime,30)),
                                           Rain=sum(Rain_mm_Tot, na.rm=TRUE)))


ros.1 <- merge(ros15_30, ros05_30)
# ros <- merge(ros.1, fcp2.sum)
ros <- ros.1
#replace below zero PAR with zero 
ros$PPFD[ros$PPFD<0] <- 0

#get CO2
# startDate = "2014-01-01"
# endDate = "2015-01-02"
Ring = 1
fn <- sprintf("R%s_FCPLOGG_R",Ring)
Rawdata <- downloadTOA5(fn, 
                        maxnfiles = 1000, 
                        rowbind=FALSE,
                        startDate = startDate,
                        endDate = endDate)      

#make all wrong format to numeric so that we can rbine
Rawdata <- lapply(Rawdata, function(x){
  x$WindSpeed <- as.numeric(as.character(x$WindSpeed))
  return(x)
  
})
CO2Data <- do.call(rbind, Rawdata)

#set limits (350-1000) for CA *baddata reports can be found in separet files
CO2Data$Concentration.1Min[CO2Data$Concentration.1Min > 1000] <- 1000
CO2Data$Concentration.1Min[CO2Data$Concentration.1Min < 350] <- 350

################################################################################################################
CO2Data$DateTime <- nearestTimeStep(CO2Data$DateTime, 30, "ceiling")

sumCO2 <- data.table(CO2Data)[,list(CO2=mean(Concentration.1Min, na.rm=TRUE),
                                    PRESS = mean(IRGA.Pressure, na.rm=TRUE),
                                    WindSpeed=mean(WindSpeed, na.rm=TRUE)
),
by = DateTime]

# plot(PRESS~DateTime,data=sumCO2,ylim=c(950,1050),pch=15,cex=0.2)
# Fill missing values
library(zoo)
sumCO2$CO2 <- na.locf(sumCO2$CO2)

sumCO2$WindSpeed[sumCO2$WindSpeed < 0] <- NA
sumCO2$WindSpeed <- na.locf(sumCO2$WindSpeed)

sumCO2$PRESS[sumCO2$PRESS < 900] <- NA
sumCO2$PRESS[sumCO2$PRESS > 1100] <- NA
sumCO2$PRESS <- na.locf(sumCO2$PRESS)
sumCO2$PRESS <- sumCO2$PRESS * 100 #times 100 to convert unit from hPa to Pa
#write the CO2 into a RDS file

CaData <- sumCO2
######################################################################################
#merge all the data 
AllData <- merge(CaData,ros,by="DateTime",all.y = TRUE)
# fill missing values
AllData$CO2 <- na.locf(AllData$CO2)
AllData$PRESS <- na.locf(AllData$PRESS)
AllData$WindSpeed <- na.locf(AllData$WindSpeed)

AllData$DOY <- yday(AllData$DateTime) 
AllData$HOD <- seq(0,47)
library(plantecophys)
AllData$vpd <- RHtoVPD(AllData$RH,AllData$Tair,Pa=AllData$PRESS/1000)
AllData$ndep <- -999.
AllData$nfix <- -999.
AllData$tsoil <- AllData$Tair
AllData$year <- year(AllData$DateTime)
  
gday.df <- AllData[,c("year","DOY","HOD","Rain","PPFD","Tair","tsoil",
                      "vpd","CO2","ndep","nfix","WindSpeed","PRESS")]
names(gday.df) <- c("year","doy","hod","rain","par","tair","tsoil",
                    "vpd","co2","ndep","nfix","wind","press")

gday.df$press <- gday.df$press/1000
write.table(gday.df,file="gdayMet.dat",quote = F,row.names = F)
