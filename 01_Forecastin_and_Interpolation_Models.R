library(readxl)
library(spdep)
library(rcompanion) #-> matrixStats -> Histogram

#############
#abline(h=1)) making trend ;line

getwd()
setwd("C:/Users/Doctor Shandu/OneDrive - University of Witwatersrand/MODULES/STAT 7006A_Spatial Stats/Project/Data/R_Stations/Work")

#Loading Data
ec_preci <- read.csv("PreciMontlyStation.csv") 
View(ec_preci) 
class(ec_preci) #checking if it is compatible with time series Data Frame -> must be converted to time series


######################################################################
#____________ START: EXPLORATORY SPATIAL DATA ANALYSIS ___________________________

#Plot Summary Stats
##Setting out the plot areas many maps in one sheet
plot.new()
frame()
par(mfcol=c(2,2))

plotNormalHistogram(ec_preci$Precipitation..mm.day., breaks = 10, main = "Annual Precipitation Histogram ",
                    labels=FALSE,col="blue", linecol="red",xlab="Precipitation (Yearly Averaged)", prob = FALSE) #(hist)
qqnorm(ec_preci$Precipitation..mm.day., main = "Precipitation Variability QQ Plot")
qqline(ec_preci$Precipitation..mm.day., col = "steelblue", lwd = 2)
boxplot(ec_preci$Precipitation..mm.day.,
        main="Precipitation Box Plot",
        xlab="Months", ylab = "Precipitation (mm/day)", col = "green")
ts.plot(ec_preci$Precipitation..mm.day., xlab="Months",ylab = "Precipitation (mm/day)",
        main="Monthly Precipitation Variability from 1900 - 2021",col = "steelblue", lwd=2 )
#abline(h=1)

#____________ END: EXPLORATORY SPATIAL DATA ANALYSIS _____________________________
###################################################################################
###################################################################################
#_____________START: ARIMA MODELLING TEST 1________________________________________

#Declaring and Converting Data to time Series
precip.ts=ts(ec_preci$Precipitation..mm.day., start = min(1990), end = max(2021), frequency = 12) #Data Frame -> must be converted to time series
class(precip.ts)
View(precip.ts)

library(forecast)
library(tseries)

#Stationarity
plot(precip.ts, ylab="Precipitation (mm/day)", xlab="Date (Monthly)", main="Monthly Precipitation Variability 1990 - 2021", 
     col="blue")
acf(precip.ts, main="Stationarity Precipitation Variability 1990 - 2021" )
pacf(precip.ts, main="Stationarity Precipitation Variability 1990 - 2021")
adf.test(precip.ts) #Viewing Augumented Dickey Fuller, p-vallues and hypothesis

#White Noise
set.seed(30)
whitenoise = ts(rnorm(273, mean = 1)) #ts from plot
plot(whitenoise, main="Whitenoise")
abline(h=1)

#precip.ts = ts(ec_preciAnn$Precipitation..mm.day., frequency = 12, start = c(1990), end = c(2021)) #Same as above time series plot
plot(precip.ts, xlab="Date (Monthly)", ylab="Precipitation (mm/day)", main="Precipitation Variability For 1990 - 2021")
abline(h=1)

lag.plot(precip.ts, lags = 9, do.line = FALSE)
lag.plot(whitenoise, lags = 9, do.line = FALSE)

# !! The model is stationary but not at other significant level


##### DIFFERENCING METHOD 2
#Taking the first Diference to remove trend ` Change month to month`
diferencing_preci <- diff(precip.ts)
##### END DIFFERENCING METHOD 2


#Plot Time differenced 
autoplot(diferencing_preci) + 
  ggtitle("Precipitation Time Variability Diffeneced") +
  ylab("Precipitation (mm/day") +
  xlab("Date")

#The Series Differenced Appear Stationary, Investigate Seasonality: yEARS AND MONTHS
ggseasonplot(diferencing_preci) + 
  ggtitle("Seasonal Change Plot: Precipitation") +
  ylab("Precipitation (mm/day") +
  xlab("Date")

#OTHER seasonal plot ` seaonal sub series plot`
ggsubseriesplot(diferencing_preci) +
  ggtitle("Seasonal Change Plot: Precipitation") +
  ylab("Precipitation (mm/day") +
  xlab("Date")

##Setting out the plot areas many maps in one sheet
plot.new()
frame()
par(mfcol=c(2,2))

#Again checking stationary After above
plot(diferencing_preci, ylab="Precipitation (mm/day)", xlab="Date (Months)", 
     main="Monthly Precipitation Variability 1990 - 2021", col="blue")
acf(diferencing_preci, main="Precipitation Series Model Residual ")
pacf(diferencing_preci, main="Precipitation Series Model Residual ")
boxplot(diferencing_preci,
        main="Monthly Precipitation boxplot from 1901 - 2021",
        xlab="Date (Months)", ylab = "Precipitation (mm/day)", col = "green") #Checking distribution
adf.test(diferencing_preci)

#_____________END: ARIMA MODELLING TEST 1____________________________________________
#####################################################################################
#_____________START: ARIMA MODELLING FORECASTING ______________________________________

library(fpp2)


#### USING BENCH MARK METHOD FORECAST - FITTING MODELS

############## BENCHMARKING 1 SNB

# Using Seasonal Naive Method as Benchmark ~ Usind Differenced Data
# y_t = y_{t-s} + e_t
fit <- snaive(diferencing_preci) #Residual sd = 1.572
print(summary(fit)) #Tell residual sd= small umber close to zero are better, RMSE, ME, MAE..
checkresiduals(fit) #ACF, All car should be inside 95% ci

############## BENCHMARKING 2 ETS

# Using ETS Exponential Smoothing Model Method as Benchmark ~ Using Differences Data
#Some can allow for trend, we can use data before difference
fit_ets <- ets(diferencing_preci) #try several method and pick the best ~ Sigma = Residual sd = 0.53
print(summary(fit_ets)) 
checkresiduals(fit_ets)

############## BENCHMARKING 3 ARIMA

# Using ARIMA Method as Benchmark ~ DATA NEED TO BE STATIONAL 
fit_arima <- auto.arima(diferencing_preci, d=1, D=1, stepwise = FALSE, 
                        approximation = FALSE, trace = TRUE) #before fitting take diffence of 1, D= seasonal Differnce, stepwise = FALSE by defauslt try to make it best try all methods and approximation 
print(summary(fit_arima))  #Residual sd = 1.145
checkresiduals(fit_arima) #Resut in Square sigma and can take it square root for standard deviation


############## FORECASTING MODEL FOR FORECAST USING BEST (USING BEST BENCHMARKING)

fcst_ets <- forecast(fit_ets, h=48) #Fit Exponential ETS  ~ h =486 months
autoplot(fcst_ets)
autoplot(fcst_ets, include = 60,main ="Precipitation Forecasted Using ETS",
         ylab="Precipitation (mm/day)", xlab="Date (Months)") #Including the last 60 months 
print(summary(fcst_ets))

fcst_arima <- forecast(fit_arima, h=48) #Fit ARIMA
autoplot(fcst_arima, main ="Precipitation Forecasted Using ARIMA 2,1,0", 
         ylab="Precipitation (mm/day)", xlab="Date (Months)")
autoplot(fcst_arima, include = 60, main ="Precipitation Forecasted Using ARIMA 2,1,0", 
         ylab="Precipitation (mm/day)", xlab="Date (Months)")
print(summary(fcst_arima))


#_____________END: ARIMA MODELLING FORECASTING ______________________________________
#####################################################################################
#####################################################################################
###__________ START: DROUGHT INDEXING: STANDARDISED PRECIPITATION INDEX______________

#Clear Variables in Work space
rm(list=ls())

library(SPEI)

#Data Loading
monthlyStationPrecipitation <- read.csv("PreciMontlyStation.csv")
monthlyStationPrecipitation

#SPI Calculation
spi3 <- spi(monthlyStationPrecipitation$Precipitation..mm.day., 3) #3 months spi
spi3
plot.spei(spi3)

spi6 <- spi(monthlyStationPrecipitation$Precipitation..mm.day., 6) #12 months spi
spi6
plot.spei(spi6)

spi9 <- spi(monthlyStationPrecipitation$Precipitation..mm.day., 9) #12 months spi
spi9
plot.spei(spi9)

spi12 <- spi(monthlyStationPrecipitation$Precipitation..mm.day., 12) #12 months spi
spi12
plot.spei(spi12)

###______________END: DROUGHT INDEXING: STANDARDISED PRECIPITATION INDEX___________
#####################################################################################
######################################################################################
###______________START: SPATIAL INTERPOLATION PREPARATION____________________________

#Clear Variables in Work space
rm(list=ls())

#MAPPING IN R PRECIPITATION
#Spatial Data Processing
library(tidyverse)
library(sf)
library(mapview)
library(sp)
library(raster)
library(gstat) #Responsible for variogram and krigin

library(devtools)
install_github("edzer/gstat")

sessionInfo()
library(automap)
#remove.packages("automap")
#install.packages("automap")

#Plotting Packages
library(patchwork)
library(viridis)
library(data.table)
library(rgdal)
library(geoR)

#Data Loading
perStationPrecipitation <- read.csv("Precipitation.csv")
perStationPrecipitation
summary(perStationPrecipitation)

#Maiking a Map #->stationPrecipitation
mapview(perStationPrecipitation, xcol = "Longitude", ycol = "Latitude", crs = 4326, grid = FALSE)

##Setting out the plot areas many maps in one sheet
plot.new()
frame()
par(mfcol=c(1,1))

#Plot and Mapping
coordinates(perStationPrecipitation) = c("Longitude", "Latitude")
crs.geo1 = CRS("+proj=longlat")
proj4string(perStationPrecipitation) = crs.geo1
plot(perStationPrecipitation, pch=20, col="blue")

#Shapefiles
ec_district = readOGR(dsn="./Shapefile/ec_district.shp")
ec_localm = readOGR(dsn = "./Shapefile/ec_localMun.shp")
class(ec_district)

#Plot with Overlay
plot(ec_district, size = 2, col = "transparent",lwd=4)
#plot(ec_localm, add=TRUE, fill=NA, lwd=1, border='red')
points(perStationPrecipitation, pch = 20, col="blue", lwd = 0.9, cex = 5)
crs(ec_district)
#Adding Legend
legend( x="bottomright", 
       legend=c("Weather Stations","District Municipalieis"), 
       col=c("blue","black", "red"), pch=c(19,0), lwd=1, lty=c(NA,NA), 
       merge=FALSE )
ggtitle("The Eastern Cape Weather Station Coverage")




#Plot with Voronoi Coverage
library(dismo)
v <- voronoi(perStationPrecipitation)
plot(ec_district, size = 2, col = "grey",lwd=1)
points(perStationPrecipitation, pch = 20, col="blue")
plot(v, add=TRUE, lwd=1)
#Adding Legend
lineLegend = c(NA,NA,1) #Creating line 
plotSym <- c(16,15,NA)
legend( x="bottomright", 
        legend=c("Weather Stations","District Municipalieis","Voronoi Polygons"), 
        col=c("blue","grey", "black"), pch=c(19,15, NA),lty=lineLegend, 
        merge=FALSE )
title("The Eastern Cape Weather Station Coverage")

#Seting Up North Arror
arrow1 <-  layout.north.arrow(type = 1)
Narrow1 <- maptools::elide(arrow1, shift = c(extent(ec_district)[1]-0.4,extent(ec_district)[2]))
plot(Narrow1, add = TRUE,col = "black")

arrowSi <- layout.north.arrow(type = 2)
NarrowSi <- maptools::elide(arrow2, shift = c(extent(ec_district)[1]-0.4,extent(ec_district)[2]))
plot(NarrowSi, add = TRUE, col = "black")


#Simple North Arrow
###Installing GISTools
uzh_gistools <- c("rgeos", "ggmap", "sf",
                  "sp","raster" , "tmaptools",
                  "rgdal", "gdalUtils", "mapview", "tmap")
install.packages(uzh_gistools)
library(GISTools) 

#Scale Bar in tools
raster::scalebar(d = 100, # distance in km
                 xy = c(extent(ec_district)[1]+0,extent(ec_district)[3]+0.00),
                 type = "bar", 
                 divs = 2, 
                 below = "km", 
                 lonlat = TRUE,
                 label = c(0,50,100), 
                 adj=c(0, -0.75), 
                 lwd = 2)

###______________END: SPATIAL INTERPOLATION PREPARATION____________________________
#################################################################################
###______________START: SPATIAL INTERPOLATION ____________________________________

#Packages
library(dplyr)
library(xlsx)
#library(raster)
library(sf)
#library(sp)
library(terra)
library(Metrics)

#Interpolation Packages
library(fields) # TPS ~ Thin Plate Spline Regression
#library(nngeo) #NN Nearest Neighbor
#library(gstat)  # Nearest Neighbour, IDW, Kriging
#library(mgcv)   # GAM ~ General Additive Model
#library(interp) # TIN ~ Triangular Irrelar Network
#library(automap)# Automatic Kriging

#____________________________TRYING METHODS___________________________________

PrecipitationInterpo <- read.csv("Precipitation.csv")
PrecipitationInterpo

#Creating Grid Boundary
boundary <- c(
  "xmin" = min(PrecipitationInterpo$Longitude),
  "ymin" = min(PrecipitationInterpo$Latitude),
  "xmax" = max(PrecipitationInterpo$Longitude),
  "ymax" = max(PrecipitationInterpo$Latitude))
  
extent_grid <- expand.grid(
  Longitude = seq(from=boundary["xmin"], to=boundary["xmax"], by=0.002),
  Latitude = seq(from=boundary["ymin"], to=boundary["ymax"], by=0.002))
  
# Output extent to raster
extent_grid_raster <- extent_grid %>%
  dplyr::mutate(Z = 0) %>%
  raster::rasterFromXYZ(
  crs = "+proj=longlat +datum=WGS84")



###____________TRYING THE TPS SPATIAL INTERPOLATION METHOD_________________________
##Setting out the plot areas many maps in one sheet
plot.new()
frame()
par(mfcol=c(2,2))

####_____________For 1995__________
TPS1995 <- Tps(x = as.matrix(PrecipitationInterpo[, c("Longitude", "Latitude")]), Y=PrecipitationInterpo$X1995, miles=FALSE)
TPS1995_intr <- interpolate(extent_grid_raster, model=TPS1995)
plot(TPS1995_intr, main = "TPS 1995 Precipitation", ylab="Latitude", xlab="Longitude")
plot(ec_district,add=TRUE, size = 2, fill=NA,lwd=1)
points(perStationPrecipitation, pch = 20, col="blue", legend=TRUE)

#Adding Legends
#legend("topleft", lty=1,  col=c("black","darkgray"), c("District", "Station"))
legend( x="topleft", 
        legend=c("Point","Line"), 
        col=c("blue","black"), pch=c(19,NA), lwd=1, lty=c(NA,1), 
        merge=FALSE )

#North Arror
library(maptools)
north(plocation = "tl", which_north = "true", style = "style_1", fill = c("black","black") )

#Scale Bar in tools
raster::scalebar(d = 100, # distance in km
                 xy = c(extent(perStationPrecipitation)[1]+0,extent(perStationPrecipitation)[3]+0.00),
                 type = "bar", 
                 divs = 2, 
                 below = "km", 
                 lonlat = TRUE,
                 label = c(0,50,100), 
                 adj=c(0, -0.75), 
                 lwd = 2)

###_____________For 2000__________
TPS2000 <- Tps(x = as.matrix(PrecipitationInterpo[, c("Longitude", "Latitude")]), Y=PrecipitationInterpo$X2000, miles=FALSE)
TPS2000_intr <- interpolate(extent_grid_raster, model=TPS2000)
plot(TPS2000_intr, main = "TPS 2000 Precipitation")
plot(ec_district,add=TRUE, size = 2, fill=NA,lwd=1)
points(perStationPrecipitation, pch = 20, col="blue", legend=TRUE)


#Adding Legends
#legend("topleft", lty=1,  col=c("black","darkgray"), c("District", "Station"))
legend( x="topleft", 
        legend=c("Point","Line"), 
        col=c("blue","black"), pch=c(19,NA), lwd=1, lty=c(NA,1), 
        merge=FALSE )

#North Arror
library(maptools)
north(plocation = "tl", which_north = "true", style = "style_1", fill = c("black","black") )

#Scale Bar in tools
raster::scalebar(d = 100, # distance in km
                 xy = c(extent(perStationPrecipitation)[1]+0,extent(perStationPrecipitation)[3]+0.00),
                 type = "bar", 
                 divs = 2, 
                 below = "km", 
                 lonlat = TRUE,
                 label = c(0,50,100), 
                 adj=c(0, -0.75), 
                 lwd = 2)

###_____________For 2006__________
TPS2006 <- Tps(x = as.matrix(PrecipitationInterpo[, c("Longitude", "Latitude")]), Y=PrecipitationInterpo$X2006, miles=FALSE)
TPS2006_intr <- interpolate(extent_grid_raster, model=TPS2006)
plot(TPS2006_intr, main = "TPS 2006 Precipitation")
plot(ec_district,add=TRUE, size = 2, fill=NA,lwd=1)
points(perStationPrecipitation, pch = 20, col="blue", legend=TRUE)


#Adding Legends
#legend("topleft", lty=1,  col=c("black","darkgray"), c("District", "Station"))
legend( x="topleft", 
        legend=c("Point","Line"), 
        col=c("blue","black"), pch=c(19,NA), lwd=1, lty=c(NA,1), 
        merge=FALSE )

#North Arror
library(maptools)
north(plocation = "tl", which_north = "true", style = "style_1", fill = c("black","black") )

#Scale Bar in tools
raster::scalebar(d = 100, # distance in km
                 xy = c(extent(perStationPrecipitation)[1]+0,extent(perStationPrecipitation)[3]+0.00),
                 type = "bar", 
                 divs = 2, 
                 below = "km", 
                 lonlat = TRUE,
                 label = c(0,50,100), 
                 adj=c(0, -0.75), 
                 lwd = 2)

###_____________For 2020__________
TPS2020 <- Tps(x = as.matrix(PrecipitationInterpo[, c("Longitude", "Latitude")]), Y=PrecipitationInterpo$X2020, miles=FALSE)
TPS2020_intr <- interpolate(extent_grid_raster, model=TPS2020)
plot(TPS2020_intr, main = "TPS 2020 Precipitation")
plot(ec_district,add=TRUE, size = 2, fill=NA,lwd=1)
points(perStationPrecipitation, pch = 20, col="blue", legend=TRUE)


#Adding Legends
#legend("topleft", lty=1,  col=c("black","darkgray"), c("District", "Station"))
legend( x="topleft", 
        legend=c("Point","Line"), 
        col=c("blue","black"), pch=c(19,NA), lwd=1, lty=c(NA,1), 
        merge=FALSE )

#North Arror
library(maptools)
north(plocation = "tl", which_north = "true", style = "style_1", fill = c("black","black") )

#Scale Bar in tools
raster::scalebar(d = 100, # distance in km
                 xy = c(extent(perStationPrecipitation)[1]+0,extent(perStationPrecipitation)[3]+0.00),
                 type = "bar", 
                 divs = 2, 
                 below = "km", 
                 lonlat = TRUE,
                 label = c(0,50,100), 
                 adj=c(0, -0.75), 
                 lwd = 2)

###_______________SPATIAL INTERPOLATION MODEL CROSS VALIDATION_____________________






#################################################################################
###_____________FOOT NOTE AND REFERENCED CODE____________________________________

#Interpolating 
#https://swilke-geoscience.net/post/2020-09-10-kriging_with_r/kriging/

#Spatial Interpolation
#https://www.analyticsvidhya.com/blog/2021/05/spatial-interpolation-with-and-without-predictors/
#https://rspatial.org/raster/analysis/4-interpolation.html

#Refere ; https://rpubs.com/richkt/269797


###______________EXTRA_______________________ 
#Spatial Data Processing
#library(gstat) #Responsible for variogram and krigin
#remove.packages("gstat")
#install.packages("gstat") #install.packages("gstat",dependencies=T)

# install.packages("devtools")
#devtools::install_github("tidyverse/readxl")















