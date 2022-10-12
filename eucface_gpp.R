rm(list=ls())
cat("\014")
# load and set the model####
source("R/load.R")
# source("R/warpar.R")

for (i in 1:6){
  #assign extwind: effciency of wind exponential decline with canopy depth 
  fn <- sprintf("Rings/Ring%s/runfolder/str.dat",i)
  replaceNameList(namelist="aero",
                  datfile=fn,
                  vals=list(extwind = 0.0))
  
  replaceNameList(namelist="lia",
                  datfile=fn,
                  vals=list(NALPHA = 1, ELP = 1.0))
}

#test - LAI sensitivity test#
test <- 1.0
# lai in the model = measured - base 
# specify test parameters 
# base <- c(0.69,0.79,0.63,0.59,0.82,0.60) # branch and stem area to be subtracted from LAI
base <- rep(0.8,6)
# # vj.ratio#
# vj.ratio.test = FALSE
# vj.ratio = 1.6

# change co2 and t test ##
co2.increase <-0 #elevated co2 test; ppm
temp.increase <- 0 # temperature increase test; celsius

for ( i in 1:6){
  fn <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
  replaceNameList(namelist="CCSCEN",datfile=fn,
                  vals=list(CO2INC = co2.increase,
                            TINC = temp.increase))
}

# chose gs model: 3: Leuning; 4 is optbb; 6 is tuzet#
# gs.model.num <- 4
# inputs for running scenarios
ca.e <- TRUE
photo.acli <- FALSE
# fire up the model####
########################################################TRUE
# check all the option above before launch the model####
########################################################
# make sure you want to do this first
time.used <- eucGPP(startDate = as.Date("2018-01-01"), 
                    endDate = as.Date("2019-12-31"), 
                    lai.test = test,
                    lai.base = base,#note that this number is not used for now and s
                    rings = 1:6,
                    model.sel = 4,
                    hourly.data = TRUE,
                    vc.vpd = TRUE,
                    vj.ratio.test = FALSE,
                    vj.ratio = 0,
                    swc.g1 = TRUE,
                    ca.change = ca.e,
                    photo.acli = photo.acli)


# stop('end here')
# analysis###################
source("R/get flux.r")

data.sap.month <- summaryBy(GPP + Ra + Trans + VPD + PAR + LAI + TAIR + PPT + APAR ~  Month + Treatment,
                      FUN=mean, na.rm = T, keep.names = T,dat=data.sap)
data.sap.month.a <- subset(data.sap.month, Treatment == "A", select = c(Month, GPP, Ra, Trans, VPD, PAR, LAI, TAIR, PPT, APAR))
data.sap.month.e <- subset(data.sap.month, Treatment == "E", select = c(Month, GPP, Ra, Trans, VPD, PAR, LAI, TAIR, PPT, APAR))

data.sap.year <- summaryBy(GPP + Ra + Trans + VPD + PAR + LAI + TAIR + PPT + APAR ~ Year + Treatment,
                            FUN=mean, na.rm = T, keep.names = T,dat=data.sap)

#make plots
#
library(ggplot2)

#Units:
# GPP and Ra = g/m2/day
# Trans + PPT precip = mm/day
# PAR + Abs PAR APAR = mJ/m2/day
# LAI = m2 m−2



#########################
#### Plotting for Ra ####
#########################
#plot for Ra~Time 
ggplot(data = data.sap, 
       mapping = aes(x = Date, y = Ra)) +
  geom_point(aes(color=Ring),size = 1)+
  theme_light()

#plot for Ra~VPD
ggplot(data = data.sap, 
       mapping = aes(x = VPD, y = Ra)) +
  geom_point(aes(color=Ring),size = 1)+
  theme_light()

#plot for Ra~Trans
ggplot(data = data.sap, 
       mapping = aes(x = Trans, y = Ra)) +
  geom_point(aes(color=Ring),size = 1)+
  theme_light()

#plot for Ra~PAR
ggplot(data = data.sap, 
       mapping = aes(x = PAR, y = Ra)) +
  geom_point(aes(color=Ring),size = 1)+
  theme_light()

#plot for Ra~APAR
ggplot(data = data.sap, 
       mapping = aes(x = APAR, y = Ra)) +
  geom_point(aes(color=Ring),size = 1)+
  theme_light()

#plot for Ra~TAIR
ggplot(data = data.sap, 
       mapping = aes(x = TAIR, y = Ra)) +
  geom_point(aes(color=Ring),size = 1)+
  theme_light()

#plot for Ra~PPT
ggplot(data = data.sap, 
       mapping = aes(x = PPT, y = Ra)) +
  geom_point(aes(color=Ring),size = 1)+
  theme_light()

# source("R/get histo.r")
# move file to output folders####
# fn.vec <- c(#"maespa trans vs hp hrly.pdf",
#             # "mastra and sap hr.rds",
#             #"mastra and sap 05hr.rds",
#             #"all.hr.rds",
#             "mastra and sap.rds",
#             # "maespa trans vs hp.pdf",
#             'histo.rds')
# 
# if(identical(photo.acli,TRUE) & identical(ca.e,TRUE)){
#   file.rename(from=fn.vec,
#               to=file.path("output","accli",fn.vec))
# }else{
#   if(identical(ca.e,TRUE)){
#     file.rename(from=fn.vec,
#                 to=file.path("output","elevated",fn.vec))
#   }else{
#     file.rename(from=fn.vec,
#                 to=file.path("output","ambient",fn.vec))
#   }
# }
# 
