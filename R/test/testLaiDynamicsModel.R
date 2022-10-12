#
lai.test.func <- function(simu.term = 365.25, #yr-1; month-12; week-52; day-365.25
                          VPD = 2.5,
                          E  = 1.4,#800/simu.term
                          PAR  = 4000/simu.term,
                          TMAX  = 26,
                          Ca = 390  ){
  # constants
  Jmax25 = 160  
  m = 0  
  P = 100  
  theta = 0.7  
  gammaS25 = 42.75  
  k = 0.5  
  Rdark = 1.59  
  SLA = 59.02  
  ll.in = 1.42 * simu.term
  mole.weight.c <- 12
  g2ummole.c <- 1/12 * 10^6
  mole.weight.ch <- 1.6
  mumole2mole <- 10^-6
  days <- 365.25/simu.term
  h <- 12 * 60 * 60
  mole2mm.water <- 1.8 * 10^-2
  construction.r.constant <- 1.3
  convertor <- 2 * 10^6
  MOL2MJ <- 4
  Q <- 2 * PAR/days
  
  
  # fraction based on Budyco curve (Zhang et al, 2001)
  # fraction of MAP available to plant
  
  fraction <- 1
  
  # gammaS value as func of TMAX (Bernacchi,2001)
  gammaS <- exp(19.02-37.83/(8.314*10^-3*(TMAX + 273.15)))
  
  Jmax <- Jmax25
  # functions to get LUE ####
  CiFunc <- function (g1) {
    Ca * g1 / (g1 + VPD^0.5)
  }
  
  #photo rate at saturate light on the top of canopy
  AxFunc <- function(g1){
    Jmax / 4.0 * (CiFunc(g1) - gammaS) / (CiFunc(g1) + 2.0 * gammaS)
  }
  
  # quntum effect (Ci function cite Belinda's 2000 CJFR paper;medlyn 2007)
  alphaFunc <- function (x){
    0.26/4.0 * (Ca - gammaS) / (Ca + 2.0 * gammaS)
  }
  
  #q=(π∙k∙α∙Q*gamma)/(2h(1-m)A_x ) Sands 1995 Eq 13
  qFunc <- function(x){
    (pi * k * alphaFunc(x) * Q * convertor) / (2.0 * h * (1.0-m) * AxFunc(x))
  }
  
  # mast mode lue
  Epsilon <- function(g1){
    qq <- qFunc(g1)
    
    sin1 <- sin(pi / 24)
    sin2 <- sin(pi * 3 / 24)
    sin3 <- sin(pi * 5 / 24)
    sin4 <- sin(pi * 7 / 24)
    sin5 <- sin(pi * 9 / 24)
    sin6 <- sin(pi * 11 / 24)
    g1 <- sin1 / (1 + qq * sin1 + sqrt((1 + qq * sin1) ^ 2 - 4 * theta * qq * sin1))
    g2 <- sin2 / (1 + qq * sin2 + sqrt((1 + qq * sin2) ^ 2 - 4 * theta * qq * sin2))
    g3 <- sin3 / (1 + qq * sin3 + sqrt((1 + qq * sin3) ^ 2 - 4 * theta * qq * sin3))
    g4 <- sin4 / (1 + qq * sin4 + sqrt((1 + qq * sin4) ^ 2 - 4 * theta * qq * sin4))
    g5 <- sin5 / (1 + qq * sin5 + sqrt((1 + qq * sin5) ^ 2 - 4 * theta * qq * sin5))
    g6 <- sin6 / (1 + qq * sin6 + sqrt((1 + qq * sin6) ^ 2 - 4 * theta * qq * sin6))
    
    #Trapezoidal rule - seems more accurate
    gg <- 1.0/6.0 * (g1 + g2 + g3 + g4 + g5 + g6)
    eps <- alphaFunc(g1) * gg * pi
    return(eps)
  }
  
  LUE <- function(g1) {
    Epsilon(g1) * MOL2MJ * mole.weight.c
  }
  
  # instrinsic tranpiration effciency as in Medlyn 2011
  ITE <- function(g1){
    1 / mole2mm.water * mole.weight.c * mumole2mole / mole.weight.ch * Ca *P / (VPD + g1 * VPD^0.5)
  }
  
  # get LAI based on E*ITE and LUE####
  LAI <- function(g1){
    -1.0/k * log(1.0 - fraction * E * ITE(g1) / LUE(g1) / PAR)
  }
  
  # basiclly mol of water times ite
  GPPfunc <- function(g1){
    LUE(g1) * PAR * (1-exp(-k*LAI(g1)))
  }
  
  # gs  mol H2O m-2 leaf s-1
  gs <- function(g1){
    mole.weight.ch * (1 + g1/VPD^0.5) * GPPfunc(g1) * g2ummole.c  / Ca / LAI(g1) / h / days
  }
  
  # construction based on SLA following Manzonni et al. 2015;
  # SLA based on Duursma et al. 2015:
  LMA <- 1.0/SLA*10^4 #g Dry mass m-2 leaf
  # LMA LL based on Wright 2005
  # RAD is W m-2 (daily average) and par is MJ m-2 yr-1 (annual total)
  # to convert PAR to RAD (1w = 1*10^-6 MJ/s):
  LCA <- 0.5*LMA  #g C m-2 leaf
  #optimasation target
  netfunc <- function(g1){
    GPPfunc(g1) -  construction.r.constant * LCA * LAI(g1) / ll.in -
      mole.weight.c * mumole2mole  * 2 * h * days * LAI(g1)* Rdark
  }
  
  
  optimal.g1 <- optimise(netfunc,interval = c(0.5,25),maximum=TRUE)$maximum
  
  return(LAI(optimal.g1))
}
library(HIEv)
library(lubridate)
library(dplyr)
library(doBy)
sap.lai.df <- readRDS("sap_lai.rds")
# startDate <- min(sap.lai.df$Date,na.rm=TRUE)
# 
# endDate <- max(sap.lai.df$Date,na.rm=TRUE)

# ROS weather data
ros05 <- downloadTOA5("ROS_WS_Table05",
                      startDate = startDate,
                      endDate = endDate,
                      topath="download")

ros05_30 <- as.data.frame(summarize(group_by(ros05[ros05$PPFD_Avg > 0,],
                                             Date),
                                           PPFD=sum(PPFD_Avg*5*60, na.rm=TRUE),
                                           Tair=mean(AirTC_Avg, na.rm=TRUE),
                                           RH=mean(RH, na.rm=TRUE)))
ros05_30 <- na.omit(ros05_30)
ros05_30$vpd <- (1-ros05_30$RH/100)*0.61375*exp(17.502*ros05_30$Tair/(240.97+ros05_30$Tair))
ros05_30$par <- ros05_30$PPFD*10^-6/4.57
# met
met.df <- ros05_30[,c("Date","vpd","par","Tair")]

sap.lai.met.df <- merge(sap.lai.df,met.df,by =c("Date"))

# 
lai.vec <- c()
for(i in 1:nrow(sap.lai.met.df)){
  lai.vec[i] <- lai.test.func(E = sap.lai.met.df$volRing[i],
                              VPD = sap.lai.met.df$vpd[i],
                              PAR  = sap.lai.met.df$par[i],
                              TMAX  = sap.lai.met.df$Tair[i])
}
sap.lai.met.df$lai.opt <- lai.vec
plot(lai.vec~c(sap.lai.met.df$LAIsmooth-0.8),
     xlim=c(0,2.5),
     ylim=c(0,2.5))
abline(a=0,b=1)
sap.lai.met.df$mon <- format(sap.lai.met.df$Date,"%m-%Y")
lai.sum.df <- summaryBy(lai.opt~mon,
                        data = sap.lai.met.df,
                        FUN=mean,
                        na.rm=TRUE,
                        id = ~Date)

sap.lai.met.df <- merge(lai.sum.df,sap.lai.met.df,by=c("Date","mon"))
plot(lai.opt.mean~Date,
     data=sap.lai.met.df,
     type="p")
points(LAIsmooth~Date,
     data=sap.lai.met.df,
     pch=15,col="red")
summary(lm(lai.vec~c(sap.lai.df$LAIsmooth-0.8)))

library(package=lubridate)

# Set Weeks number. Date already of class `Date`
sap.lai.df$Week <- format(sap.lai.df$Date,"%m-%Y")