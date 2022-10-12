saturate.vp.func <- function(Tc,a=6.12,m=7.59,Tn=240.73){
  # T = Temperature (°C)
  VPS <- a*10^(m*Tc/(Tc+Tn))
  return(VPS)
}

make_met <- function(startDate= NULL,endDate = NULL){
  
#if function for if 'ca.df.rds' met data file exists, to not redo calcs - saves time  
  if(!file.exists('cache/ca.df.rds')){
    #source('r/get met.R')
  get.met.func.warp(startDate =startDate,endDate=endDate)
  }
  
  met.in <- readRDS('cache/ca.df.rds')
  met.in <- met.in[met.in$Date <= as.Date(endDate) & met.in$Date >= as.Date(startDate),]
  
  return(met.in)
#   # # choose only the days with lai######################################################################################
#   # DATES <- sm[[Ring]]$Date
#   # 
#   # if(is.null(startDate)){
#   #   startDate <- DATES[1]
#   # } else {
#   #   startDate <- startDate
#   # }
#   # 
#   # if(is.null(endDate)){
#   #   endDate <- DATES[length(DATES)]
#   # } else {
#   #   endDate <- as.Date(endDate)
#   # }
# 
#   # ROS weather data#####
#   # "FACE_R4_T1_Rain"
#   # instead of ecuface met data, the model uses met data from ROS
#   # i have tested rainfall and there is no difference at all
#   # other paramteres should be the same
#   
#   ros15 <- downloadTOA5("ROS_WS_Table15",
#                         startDate = startDate,
#                         endDate = endDate)
#   ros05 <- downloadTOA5("ROS_WS_Table05",
#                         startDate = startDate,
#                         endDate = endDate)
#   # startDate = "2014-01-01"
#   # endDate = "2014-01-02"
#   # fcp <- downloadTOA5(c("FACE","FCPLOGG"), startDate=startDate, endDate=endDate,
#   #                     maxnfiles=500, tryoffline=TRUE, quiet=TRUE)
#   # 
#   # temp1 <- suppressWarnings(thicken(fcp, "30 min", by="DateTime", colname="DateTime2"))
#   # temp2 <- mutate(temp1,Ring = Plot)
#   # fcp2 <- subset(temp2[temp2$Ring == Ring,],select=c("DateTime2","WindSpeed","IRGA.Pressure"))
#   #   
#   # fcp2.sum <- summaryBy(WindSpeed+IRGA.Pressure~DateTime2,
#   #                       data=fcp2,FUN=mean,na.rm=TRUE,keep.names=TRUE)
#   # 
#   # names(fcp2.sum) <- c("DateTime","WindSpeed", "air_pressure")
#   
#   ros05_30 <- as.data.frame(dplyr::summarize(group_by(ros05,DateTime=nearestTimeStep(DateTime,30)),
#                                              PPFD=mean(PPFD_Avg, na.rm=TRUE),
#                                              Tair=mean(AirTC_Avg, na.rm=TRUE),
#                                              RH=mean(RH, na.rm=TRUE)))
#   ros15_30 <- as.data.frame(dplyr::summarize(group_by(ros15,DateTime=nearestTimeStep(DateTime,30)),
#                                              Rain=sum(Rain_mm_Tot, na.rm=TRUE)))
#   
#   ros.1 <- merge(ros15_30, ros05_30)
#   # ros <- merge(ros.1, fcp2.sum)
#   ros <- ros.1
# 
#   #get CO2 ######################################################################################
# startDate = "2013-12-01"
# endDate = "2013-12-31"
# # Ring=4
#   fn <- sprintf("R%s_FCPLOGG_R",Ring)
#   Rawdata <- downloadTOA5(fn, 
#                           maxnfiles = 600, 
#                           rowbind=FALSE,
#                           startDate = startDate,
#                           endDate = endDate)      
#   
#   #make all wrong format to numeric so that we can rbine
#   Rawdata <- lapply(Rawdata, function(x){
#     x$WindSpeed <- as.numeric(as.character(x$WindSpeed))
#     return(x)
#     
#   })
#   CO2Data <- do.call(rbind, Rawdata)
# 
#   #set limits (350-1000) for CA *baddata reports can be found in separet files
#   CO2Data$Concentration.1Min[CO2Data$Concentration.1Min > 1000] <- 1000
#   CO2Data$Concentration.1Min[CO2Data$Concentration.1Min < 350] <- 350
# 
#   CO2Data$DateTime <- nearestTimeStep(CO2Data$DateTime, 30, "ceiling")
#   
#   CO2Data$WindSpeed[CO2Data$WindSpeed < 0] <- NA
#   
#   CO2Data$IRGA.Pressure[CO2Data$IRGA.Pressure < 900] <- NA
#   CO2Data$IRGA.Pressure[CO2Data$IRGA.Pressure > 1100] <- NA
# 
#   sumCO2 <- data.table(CO2Data)[,list(CO2=mean(Concentration.1Min, na.rm=TRUE),
#                                       PRESS = mean(IRGA.Pressure, na.rm=TRUE),
#                                       WindSpeed=mean(WindSpeed, na.rm=TRUE)#,
#                                       # PPFD=mean(PPFD, na.rm=TRUE),
#                                       # Tair=mean(Air.Temp, na.rm=TRUE),
#                                       # RH=mean(IRGA.Vapor.Pressure/saturate.vp.func(Air.Temp), na.rm=TRUE)
#                                       ),
#                                 by = DateTime]
#   
#   # # there is an issue with R4 sensor on 2013-12-18
#   # sumCO2$Tair[as.Date(sumCO2$DateTime) == as.Date('2013-12-28')] <- rep(c(18.56816,18.55167,18.02500,17.30000,
#   #                                                                     17.11167,17.08667,17.86333,19.48667,
#   #                                                                     20.82500,22.13833,23.53000,24.76333,
#   #                                                                     25.90833,27.16667,28.20333,29.24667,
#   #                                                                     29.90167,28.93333,26.88833,24.88000,
#   #                                                                     23.41000,22.63667,21.81833,20.89667),each=2)
# #,Fill,missing,values
#   library(zoo)
#   sumCO2$CO2 <- na.locf(sumCO2$CO2)
#   # sumCO2$WindSpeed[sumCO2$WindSpeed < 0] <- NA
#   # sumCO2$PPFD[sumCO2$PPFD < 0] <- 0
#   # sumCO2$PPFD <- na.locf(sumCO2$PPFD)
#   # plot(sumCO2$PPFD~sumCO2$DateTime)
#   # sumCO2$WindSpeed[sumCO2$WindSpeed < 0] <- NA
#   sumCO2$WindSpeed <- na.locf(sumCO2$WindSpeed)
#   
#   sumCO2$PRESS[sumCO2$PRESS < 900] <- NA
#   sumCO2$PRESS[sumCO2$PRESS > 1100] <- NA
#   sumCO2$PRESS <- na.locf(sumCO2$PRESS)
#   sumCO2$PRESS <- sumCO2$PRESS * 100 #times 100 to convert unit from hPa to Pa
#   # sumCO2$Tair[sumCO2$Tair>50] <- NA
#   # sumCO2$Tair <- na.locf(sumCO2$Tair)
#   # sumCO2$RH[sumCO2$RH>100] <- 100
#   # sumCO2$RH <- na.locf(sumCO2$RH)
#   CaData <- sumCO2
#   # plot(CaData$RH~CaData$DateTime)
#   #merge all the data ######################################################################################
#   AllData <- merge(CaData,ros,by="DateTime")
#   
#   # Make sure there are no gaps!######################################################################################  
#   # Merge with full timeseries...
#   met <- data.frame(DateTime=seq.POSIXt(min(ros$DateTime), max(ros$DateTime), by="30 min"))
#   met <- merge(met, AllData, all=TRUE)
#   
#   met$WindSpeed <- na.locf(met$WindSpeed)
#   met$PRESS <- na.locf(met$PRESS)
#   # RH is 0-1
#   met$RH <- met$RH / 100
#   
#   names(met) <- c("Date","CA","PRESS","WIND",
#                   "PPT","PAR","TAIR","RH")
#   met$Date <- as.Date(met$Date)

  }

#run_maespa_eucface########################################################################################
run_maespa_eucface <- function(ring,runfolder.path,
                               startDate= NULL,
                               endDate=NULL,
                               hourly.data=FALSE,
                               model.num,
                               vc.vpd,
                               fix.ca,
                               CA.in,
                               ca.change){

  o <- getwd()
  on.exit(setwd(o))

  # Smoothed LAI
  lais <- sm[[ring]]$LAIsmooth
  
  # # Calculate individual tree leaf areas.
  # DATES <- format(sm[[ring]]$Date, "%d/%m/%y")
  # 
  # # set s and e days 
  # if(is.null(startDate)){
  #   startDate <- DATES[1]
  # } else {
  #   startDate <- as.Date(startDate)
  # }
  # 
  # if(is.null(endDate)){
  #   endDate <- DATES[length(DATES)]
  # } else {
  #   endDate <- as.Date(endDate)
  # }
  #make met file
  # met <- list()
  
  met <- make_met(startDate = startDate,
                          endDate = endDate#,
                          # fix.ca=fix.ca,
                          # CA.in=CA.in
  )
  
  
  #get initial swc from hiev -- this bit is not doing anything
  
  swc.df <- downloadTOA5(sprintf("FACE_R%s_B1_SoilVars",ring),
                         startDate = startDate,
                         endDate = endDate, maxnfiles = 1000)

  swc.df <- subset(swc.df,select = c("Date",
                                     "DateTime",
                                     "Theta5_1_Avg","Theta5_2_Avg",
                                     "Theta30_1_Avg","Theta30_2_Avg",
                                     "Theta75_1_Avg","Theta75_2_Avg"))
  swc.df <- swc.df[order(swc.df$DateTime),]
  swc.df$swc.0.30 <- (swc.df$Theta5_1_Avg + swc.df$Theta5_2_Avg + swc.df$Theta30_1_Avg +swc.df$Theta30_2_Avg)/4

  swc.df$swc.30.75 <- (swc.df$Theta75_1_Avg + swc.df$Theta75_2_Avg)/2
  
  # swc.df <- readRDS('cache/swc.day.df.rds')
  
  # swc.neutron.ring.df <- readRDS('cache/swc.rds')
  
##############################################
### make sure to use the uptodate maespa #####
##############################################
  # 
  # # file.copy('maespa exe/i_histo_m.exe',runfolder.path,overwrite = TRUE)
  # file.copy('C:/Users/90948066/source/repos/maespa/Release/maespa.exe',runfolder.path,overwrite = TRUE)
  # # file.copy('C:/Users/magnolia/source/repos/maespa/Release',runfolder.path,overwrite = TRUE)
  # 
  #C:\Users\magnolia\source\repos\maespa\x64\Release
  setwd(runfolder.path)
  # Set simulation dates for given time
  replaceNameList("dates","confile.dat", vals=list(startdate=format(as.Date(startDate), "%d/%m/%y"),
                                                   enddate=format(as.Date(endDate), "%d/%m/%y")))
  
  # set up understoru files 
  replaceNameList("SPECIES","confile.dat", vals=list(nspecies = 1,
                                                     speciesnames = c('canopy'),
                                                     phyfiles = c('phy.dat'),
                                                     strfiles = c('str.dat')))
  
  if(hourly.data == TRUE) {
    hourly.T <- 1
    } else {
    hourly.T <- 0
    }
  
  replaceNameList("control","confile.dat",vals=list(iohrly = hourly.T,
                                                    iotutd  = 7,
                                                    ioresp  = 0,
                                                    iohist = 1,
                                                    IPOINTS =0))


  # max zenith angle
  replaceNameList("diffang","confile.dat",vals=list(nolay = 6,
                                                    nzen = 20,
                                                    naz= 20))
  
  replaceNameList("HISTO","confile.dat",vals=list(BINSIZE =  25))

  # change simulation for TUzet model and add parameteres
  # replacePAR("confile.dat", "modelgs","model", newval = model.num)
  replaceNameList("model","confile.dat",vals=list(modelgs = 4,
                                                  modelrd = 0,
                                                  modeljm = 0,
                                                  itermax = 200,
                                                  modelss = 0))
  
  # # initial swc from Hiev
  # replacePAR("watpars.dat", "initwater","initpars", newval=c(swc.df$swc.tdr.30[1]/100,swc.df$swc.tdr.75[1]/100))
  # # throughfall from Teresa's draft should be calculated from Tfall and rainfall measurements
  # replacePAR("watpars.dat", "throughfall","wattfall", newval = 0.96)
  # # Need to guess from throughfall and rainfall
  # replacePAR("watpars.dat", "maxstorage","wattfall", newval = 0.2)
  # 
  # # root propery based on Juan's data set
  # # look at warpar.R for details
  # replaceNameList("rootpars","watpars.dat", vals=list(rootrad = 0.0001,#m
  #                                                     # rootdens = 500000,
  #                                                     # rootmasstot = 1000,
  #                                                     # nrootlayer = 9,
  #                                                     # fracroot = c(0.1,0.001,0.199,0.2,0.1,0.1,0.1,0.1,0.1)
  #                                                     # rootrad =0.0005,
  #                                                     rootdens = 5e5, #g m^3
  #                                                     rootmasstot = root.total[ring],
  #                                                     nrootlayer = layers.num,
  #                                                     fracroot = f.vec
  #                                                     ))
  # # plant hydro conductance
  # replaceNameList("plantpars","watpars.dat", vals=list(MINLEAFWP = -3.2,#lowestvalue from WP Teresa
  #                                                      MINROOTWP = -3,
  #                                                      PLANTK = 2.68 #fit to spots; leaf-specific (total) plant hydraulic conductance (mmol m-2 s-1 MPa-1)
  # ))
  # 
  # 
  # 
  # 
  # # key is porefraction which doesn't really change according to Cosby 1984
  # replaceNameList("laypars","watpars.dat", vals=list(nlayer = layers.num + 6,
  #                                                    laythick = diff(depth.v)/100,
  #                                                    porefrac = c(0.42,0.40),
  #                                                    # porefrac = c(0.3,0.3),
  #                                                    fracorganic = c(0.8,0.2,0.1,0.02)
  #                                                    ))
  # 
  # # key is usestand = 0 which mean only consider target trees
  # replaceNameList("watcontrol","watpars.dat", vals=list(keepwet = 0,
  #                                                       SIMTSOIL = 1,
  #                                                       reassignrain = 0,
  #                                                       retfunction = 1,
  #                                                       equaluptake = 0,
  #                                                       WSOILMETHOD = 1,
  #                                                       usemeaset = 0,
  #                                                       usemeassw = 0,
  #                                                       USESTAND = 0
  #                                                       ))
  # # par value from duursma 2008
  # replaceNameList("soilret","watpars.dat", vals=list(bpar=c(4.26,6.77),
  #                                                    psie = c(-0.00036,-0.00132),
  #                                                    ksat = c(79.8,25.2)
  #                                                    ))


  # Toss met data before which we don't have LAI anyway (makes files a bit smaller)
  # met <- met#[met$Date >= min(sm[[ring]]$Date),]
  # write.csv(met,"met_ListOfAllVaule.csv")

  
  # if(ca.change == FALSE){ 
  #   if(ring %in% c(1,4,5)){
  #   met <-  subset(met,select = -c(Ca.A))
  #   names(met)[names(met) ==  'Ca.E'] <- 'CA'
  # }else{
  #   met <-  subset(met,select = -c(Ca.E))
  #   names(met)[names(met) ==  'Ca.A'] <- 'CA'
  # }
  #   }
  # 
  # if(ca.change == TRUE){
  #   if(ring %in% c(2,3,6)){
  #     met <-  subset(met,select = -c(Ca.A))
  #     names(met)[names(met) ==  'Ca.E'] <- 'CA'
  #   }else{
  #     met <-  subset(met,select = -c(Ca.E))
  #     names(met)[names(met) ==  'Ca.A'] <- 'CA'
  #   }
  # }
  
  
  if(ca.change == TRUE){
      met <-  subset(met,select = -c(Ca.A))
      names(met)[names(met) ==  'Ca.E'] <- 'CA'

  }else{
    met <-  subset(met,select = -c(Ca.E))
    names(met)[names(met) ==  'Ca.A'] <- 'CA'
  }
  # # put in soil water content
  # swc.ring.df <- swc.df[swc.df$Ring == paste0('R',ring),]
  # swc.ring.df <- swc.ring.df[,c('Date','swc.tdr')]
  # names(swc.ring.df) <- c('Date','SW')
  # met.sw <- merge(met,swc.ring.df,by='Date',all.x=TRUE)
 
  metnodate <- subset(met, select = -Date)
  
  # metnodate <- met
  #
  #fill missing value
  # c("Date","CA","PRESS","WIND",
  #   "PAR","TAIR","RH","PPT")
  # metnodate$WIND <- na.locf(metnodate$WIND)
  # metnodate$PRESS <- na.locf(metnodate$PRESS)
  # metnodate$PPT <- na.locf(metnodate$PPT)
  # metnodate$PAR <- na.locf(metnodate$PAR)
  # 
  # metnodate$TAIR <- na.locf(metnodate$TAIR)
  # metnodate$RH <- na.locf(metnodate$RH)
  # metnodate$CA <- na.locf(metnodate$CA)
  
  if(fix.ca==TRUE){metnodate$CA = CA.in}
  
  metnodate$PAR[metnodate$PAR < 0] <- 0
  # # # place in met.dat
  replacemetdata(metnodate, "met.dat", columns=names(metnodate),
                 newmetfile="met.dat", khrs=48, setdates = FALSE)
  # set date range
  replacePAR("met.dat", "startdate", "metformat", format(as.Date(startDate), "%d/%m/%y"))
  
  replacePAR("met.dat", "enddate", "metformat", format(as.Date(endDate), "%d/%m/%y"))
  
  replaceNameList("ENVIRON","met.dat", vals=list(difsky = 0.0,
                                                 ca = 390,
                                                 SWMAX = 42,
                                                 SWMIN = 0
  ))
  # set the cords
  replaceNameList("latlong","met.dat", vals=list(lat = c(33,35,49),
                                                  long = c(150,45,10),
                                                  tzlong = 150,
                                                  lonhem = 'E',
                                                  lathem = 'S'
  ))
  
  print('met updated')
  
  # run maespa
  # print(sprintf("Ring %s start",ring))
  # if(identical(TRUE,vc.vpd)){
  #   shell("i_histo_m.exe")
  # }else{
  #   shell("maespa_ori.exe")
  # }
  shell('maespa.exe')
  print(sprintf("Ring %s finished",ring))
}

#eucGPP####
eucGPP <- function(hourly.data = FALSE,startDate= NULL,endDate = NULL,rings = 1:6,model.sel = 4,vc.vpd = FALSE,
                   vj.ratio.test = FALSE,
                   vj.ratio = 1.6,
                   fix.ca=FALSE,
                   CA.in=400,
                   o = getwd(),
                   swc.g1=TRUE,
                   ca.change =FALSE,
                   photo.acli = FALSE,
                   ...){
  
  startDate<<- startDate
  endDate<<- endDate
  time.start <- Sys.time()
  update.tree.f(...,
                startDate= startDate,
                endDate = endDate)
  # update.phy.f(vj.ratio.test = vj.ratio.test,
  #              vj.ratio = vj.ratio,
  #              swc.g1=swc.g1,
  #              photo.acli = photo.acli,
  #              ...)
  
    for (ring in rings){
    run_maespa_eucface(ring = ring,
                       runfolder.path = file.path(o,sprintf("Rings/Ring%s",ring),"runfolder/"),
                       hourly.data = hourly.data,
                       startDate= startDate,
                       endDate = endDate,
                       model.num = model.sel,
                       vc.vpd = vc.vpd,
                       fix.ca=fix.ca,
                       CA.in=CA.in,
                       ca.change = ca.change
                       )

  }
  time.used <- Sys.time() - time.start
  print(time.used)
  return(time.used)
}

