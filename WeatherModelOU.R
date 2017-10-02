library(weatherData)
library(ggplot2)
library(scales)
library(plyr)

library(xts)
library(sde)

#IASI             LRIA
#PLOVDIV          LBPD
#SOFIA (OBSERVATO LBSF
#More available at: http://weather.rap.ucar.edu/surface/stations.txt

#wdata <- getWeatherForMultipleYears("LBPD", 2000, 2017,
#                           station_type = "airportCode",
#                           opt_detailed = FALSE,
#                           opt_write_to_file = FALSE)
#
#wdata$shortdate <- strftime(wdata$Date, format="%m-%d")
#wtemp <- wdata[,c("Date", "Mean_TemperatureC")]
#wtser <- xts(wtemp[,-1], order.by=as.Date(wtemp[,1], "%Y-%m-%d"))

wdata <- read.csv("I:/2017/Iasi/LBPD.csv", head=T)
wtemp <- wdata[,c("Date", "t_avg")]
wtemp <- na.omit(wtemp)
wtser <- xts(wtemp[,-1], order.by=as.Date(wtemp[,1], "%Y-%m-%d"))


wtser <- na.omit(wtser)

plot(wtser)

OUCal <-function(S)
{
  n <- length(S)
  delta <- 1
  
  Sx <- sum(S[1:length(S)-1])
  Sy <- sum(S[2:length(S)])
  Sxx <- crossprod(S[1:length(S)-1], S[1:length(S)-1])
  Sxy <- crossprod(S[1:length(S)-1], S[2:length(S)])
  Syy <- crossprod(S[2:length(S)], S[2:length(S)])
  
  a  <- (n*Sxy - Sx * Sy ) / ( n * Sxx - Sx^2 )
  b  <- (Sy - a * Sx ) / n
  sd <- sqrt((n * Syy - Sy^2 - a * (n * Sxy - Sx * Sy)) / (n * (n-2)))
  
  lambda <- -log(a)/delta
  mu <- b/(1-a)
  sigma <- sd * sqrt( -2*log(a)/delta/(1-a^2))
  c(lambda, mu, sigma)
}

# Euler Maruyama simulation method
OUSim <- function(T,n,nu,lambda,sigma,x0){
  dw  <- rnorm(n, 0, sqrt(T/n))
  dt  <- T/n
  x <- c(x0)
  for (i in 2:(n+1)) {
    x[i]  <-  x[i-1] + lambda*(nu-x[i-1])*dt + sigma*dw[i-1]
  }
  return(x);
}

OURaw <- as.numeric(wtser)

OUPlovdiv <- OUCal(OURaw)

OUData <- OUSim(length(OURaw), length(OURaw), OUPlovdiv[2], OUPlovdiv[1], OUPlovdiv[3], OURaw[1])

plot(index(wtser), OUData[-1], col="blue")
lines(index(wtser),OURaw, col="red")

MCCount <- 100

cl <- rainbow(50)

plot(as.zoo(wtser))

lines(index(wtser), OUData[-1], col="blue")
for(i in 1:MCCount) {
  OUData <- OUSim(length(OURaw), length(OURaw), OUPlovdiv[2], OUPlovdiv[1], OUPlovdiv[3], OURaw[1])
  lines(index(wtser), OUData[-1], col=cl[i%%49+1])
}

MCCount <- 10000

strike <- 18
r <- 0.01/365
T <- 90

OUData <- OUSim(90, 90, OUPlovdiv[2], OUPlovdiv[1], OUPlovdiv[3], OURaw[length(OURaw)])
plot(OUData, type="l")

# Valuation of option with arithmetic average
Aavg <- array(0,dim=c(0,MCCount))
Aoption.call <- array(0,dim=c(0,MCCount))
Aoption.put <- array(0,dim=c(0,MCCount))

# Valuation of option with geometric average
Gavg <- array(0,dim=c(0,MCCount))
Goption.call <- array(0,dim=c(0,MCCount))
Goption.put <- array(0,dim=c(0,MCCount))

for(i in 1:MCCount) {
  OUData <- OUSim(90, 90, OUPlovdiv[2], OUPlovdiv[1], OUPlovdiv[3], OURaw[length(OURaw)])
  #lines(OUData[-1], col=cl[i%%49+1])
  
  #Calculate arithmetic average
  Aavg[i]=mean(OUData)
  Aoption.call[i]=exp(-r*T)*(max((Aavg[i]-strike),0))
  Aoption.put[i]=exp(-r*T)*(max((-(Aavg[i]-strike)),0))
  
  #Calculate geometric average
  Gavg[i]=exp(mean(log(OUData)))
  Goption.call[i]=exp(-r*T)*(max((exp(Gavg[i])-strike),0))
  Goption.put[i]=exp(-r*T)*(max((-(exp(Gavg[i])-strike)),0))  
}

# Arithmetic average call option
(arithmetic.asian.call=mean(Aoption.call))
# Arithmetic average put option
(arithmetic.asian.put=mean(Aoption.put))

# Geometric average call option
(geometric.asian.call=mean(Goption.call))
# Geometric average put option
(geometric.asian.put=mean(Goption.put))


# 
bs <- function(data, indices) {
  d <- data[indices] # allows boot to select sample 
  OUB <- OUCal(d)
  return(OUB[1]) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=OURaw, statistic=bs, 
                R=10000)
