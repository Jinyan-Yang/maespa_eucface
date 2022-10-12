

stem.area <- 0.8
par(mfrow=c(2,1), tcl=-0.7, family="Date", mai=c(0.5,0.8,0.2,0.2))

palette(c("red","lightskyblue","navy","coral","orange","blue"))

plot(LAIsmooth-stem.area~Date,data = sm[[1]],type="l",ylim=c(0.4,1.6),col=1, ylab = "LAI", xlab = "Year")
for(i in 2:6){
  points(LAIsmooth-stem.area~Date,data = sm[[i]],type="l",col=i)
}
legend("top",legend = paste0("R",1:6),bty='n',col=palette(),pch=16,
       xpd = TRUE,horiz = TRUE)

for (i in 1:6){
  sm[[i]]$Ring = i
}
lai.df <- Reduce(function(x, y) merge(x, y, all=TRUE), sm)

lai.df$C_treat = NA

lai.df$C_treat[lai.df$Ring %in%c(1,4,5)] = "E"
lai.df$C_treat[lai.df$Ring %in%c(2,3,6)] = "A"

library(doBy)
lai.sum <- summaryBy(LAIsmooth~Date + C_treat,
                     data = lai.df,
                     FUN=c(mean,sd),na.rm=TRUE)

tran.func <- function(cols,alpha=0.1){
  targe.vec <- col2rgb(cols)
  out.col <- apply(targe.vec,2,FUN = function(x){
    rgb(red = x["red"]/255, green=x["green"]/255, blue=x["blue"]/255,alpha=0.1)})
  return(out.col)
}

plot(LAIsmooth.mean-stem.area~Date,data = lai.sum[lai.sum$C_treat == "E",],
     type='l',col="red",
     ylim=c(0.4,1.6), xlab = "Year", ylab = "Mean LAI")
polygon(x = c(lai.sum[lai.sum$C_treat == "E",]$Date,
              rev(lai.sum[lai.sum$C_treat == "E",]$Date)),
        y = c(c(lai.sum[lai.sum$C_treat == "E",]$LAIsmooth.mean-stem.area
                -lai.sum[lai.sum$C_treat == "E",]$LAIsmooth.sd),
              rev(lai.sum[lai.sum$C_treat == "E",]$LAIsmooth.mean-stem.area
                  +lai.sum[lai.sum$C_treat == "E",]$LAIsmooth.sd)),
        col = tran.func("red"),
        border = NA)
points(LAIsmooth.mean-stem.area~Date,data = lai.sum[lai.sum$C_treat == "A",],
       type='l',col="blue")

polygon(x = c(lai.sum[lai.sum$C_treat == "A",]$Date,
              rev(lai.sum[lai.sum$C_treat == "A",]$Date)),
        y = c(c(lai.sum[lai.sum$C_treat == "A",]$LAIsmooth.mean-stem.area
                -lai.sum[lai.sum$C_treat == "A",]$LAIsmooth.sd),
              rev(lai.sum[lai.sum$C_treat == "A",]$LAIsmooth.mean-stem.area
                  +lai.sum[lai.sum$C_treat == "A",]$LAIsmooth.sd)),
        col = tran.func("blue"),
        border = NA)
