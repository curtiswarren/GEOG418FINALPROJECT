#VERY MESSY. Workflow is somewhat in this order, though nearing the end while creating outputs things got quite messy.
#My apologies.

#****TITLES NOT ALWAYS REPRESENTATIVE OF TASK THEY ARE EXECUTING...

# Data Prep ---------------------------------------------------------------
install.packages("sf")
install.packages("plyr")
install.packages("dplyr")
install.packages("spdep")
install.packages("GISTools")
install.packages("raster")
install.packages("maptools")
install.packages("rgdal")
install.packages("spatstat")
install.packages("sp")
install.packages("tmap")
install.packages("gstat")
install.packages("spgwr")
install.packages("fBasics")
install.packages("gridExtra")
install.packages("gtable")
install.packages("grid")
install.packages("ggplot2")



#Libraries
library(sf)
library(plyr)
library(dplyr)
library(spdep)
library(GISTools)
library(raster)
library(maptools)
library(rgdal)
library(spatstat)
library(sp)
library(spatstat)
library(tmap)
library(gstat)
library(spgwr)
library(fBasics)
library(gridExtra)
library(gtable)
library(grid)
library(ggplot2)


#Set working directory
setwd("/Users/curtiswarren/Desktop/GEOG418/Assignments/Final Project/Working/geog418-518-2019-finalproject-master")

#Reading in particulate matter dataset
pm25 <- read.csv("PM25.csv") #Read in PM2.5 data
#Select only columns 1 and 2
pm25 <- pm25[,1:2]
#Change the column names 
colnames(pm25) <- c("POSTALCODE", "PM25")
pm25 <- na.omit(pm25)
View(pm25)

#Reading in postal code shapefile
postalcodes <- shapefile("./BC_PostalCodes/BC_Postal_Codes") #Read in related postal code data
proj4string(postalcodes)
postalcodes <- spTransform(postalcodes, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#Reading in dissemination tract and income data
income <- read.csv("Income.csv") #Read in census income data  
colnames(income) <- c("DAUID", "Income") #Select only ID and Income columns
census.tracts <- shapefile("./BC_DA/BC_DA.shp") #Read in dissemination tract shapefile
crs(census.tracts)
census.tracts <- spTransform(census.tracts, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
income.tracts <- merge(census.tracts,income, by = "DAUID") #Merge income and dissemination data
nrow(income.tracts) #Determine the number of columns in the dataframe
income.tracts <- income.tracts[!is.na(income.tracts$Income),]


#Select postal codes that fall within dissemination tracts)
postalcodes <- intersect(postalcodes,income.tracts)
plot(postalcodes) #See what the data looks like spatially
head(postalcodes) #See what the data looks like in tabular form

#Join PM2.5 data with postal code data
pm25.spatial <- merge(postalcodes,pm25,by = "POSTALCODE")

#Aggregate the PM2.5 values in each DA in order to have a single value per DA. Here we aggregate based on the mean.
pm25.aggregate <- aggregate((as.numeric(pm25.spatial$PM25)/10)~pm25.spatial$DAUID,FUN=max)

#Re-join aggregated data to the income.tracts layer.
colnames(pm25.aggregate) <- c("DAUID", "PM25AGG") #Select only ID and Income columns
income.pm25 <- merge(income.tracts,pm25.aggregate, by = "DAUID") #Merge income and dissemination data

View(income.pm25@data)
#Re-join aggregated data to the pm25.spatial points layer.
#THIS IS WHERE WE HAVE PM25 AND INCOME IN ONE PLACE, TAKE SUBSET FROM HERE 
pm25.points.aggregate <- merge(pm25.spatial, pm25.aggregate, by = "DAUID")
mean(pm.income.poly$Income)
mean(pm.income.poly$PM25)
sd(pm.income.poly$Income)
sd(pm.income.poly$PM25)

View(pm25.points.aggregate@data)
#Create a subsample of the datapoints provided in the PM2.5 dataset using the sample n provided on CourseSpaces
#set.seed creates a set random sample
set.seed(390)
sampleSize=390
spSample <- pm25.points.aggregate[sample(1:length(pm25.points.aggregate),sampleSize),]
plot(spSample)
View(spSample@data)

#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(spSample, "regular", n=20000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(spSample)


########################

# MORANS I FOR MEDIAN INCOME ----------------------------------------------

tmap_mode("view")
tmap_mode("plot")

map_mdincome <- tm_shape(income.pm25) + 
  tm_polygons(col = "Income", 
              title = "Median Income\n (CAD$)", 
              style = "jenks", 
              palette = "-RdBu", n = 6, lwd = 0.2)+
  tm_legend(legend.outside = FALSE,legend.position = c("LEFT","BOTTOM"), legend.title.size = 0.75,legend.text.size = 0.65)+
  #tm_layout(frame = TRUE, main.title = "Median Income per Dissemination Area in Greater Vancouver", main.title.size = 1.0, main.title.position = 0.13)+
  tm_scale_bar(width = 0.16, position = c("LEFT", "BOTTOM"))+
  tm_compass( position = c("RIGHT", "TOP"))

map_mdincome

tmap_mode("plot")

#QUEENS
van.nb <- poly2nb(income.pm25)
van.net <- nb2lines(van.nb,coords=coordinates(income.pm25))


tm_shape(income.pm25) + 
  tm_borders(col='lightgrey') + 
  tm_shape(van.net) + 
  tm_lines(col='red')

########################

van.lw <- nb2listw(van.nb, zero.policy = TRUE, style = "W")
print.listw(van.lw, zero.policy = TRUE)

########################

income.pm25$IncLagMeans = lag.listw(van.lw, income.pm25$Income, zero.policy = TRUE)
income.pm25$diflagmeans = abs(income.pm25$Income - income.pm25$IncLagMeans)

map_LagMean <- tm_shape(income.pm25) + 
  tm_polygons(col = "diflagmeans", 
              title = "Median Income\nLagged Means", 
              style = "fisher", 
              palette = "-RdBu", n = 6, lwd = 0.5) 
map_LagMean

########################

mi <- moran.test(income.pm25$Income, van.lw, zero.policy = TRUE, alternative = "two.sided")
mi

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(van.lw)

Morans.I <- mi$estimate[[1]]
Expected.I <- mi$estimate[[2]]
Variance <- mi$estimate[[3]]

Z.Score <- (Morans_I - Expected_I) / sqrt(Variance)

Morans.I <- round(Morans_I, digits = 3)

Expected.I <- round(Expected_I, digits = 5)

Variance <- round(Variance, digits = 5)

Z.Score <- round(Z, digits = 3)

data.for.table2 = data.frame(Morans.I, Expected.I, Variance, Z.Score)


#Make table for Global Morans I
columns <- c("Moran's I", "Expected I", "Variance", "Z Score")
table2 <- tableGrob(data.for.table2) #make a table "Graphical Object" (GrOb) 
t2Caption <- textGrob("Table 2: Global Moran's I Results For Median Income", gp = gpar(fontsize = 08))
padding <- unit(5, "mm")

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) +1)


grid.arrange(table2, newpage = TRUE)


#Printing a table 
png("MdIncome_Global_Moran.png", pointsize=10, width=4000, height=2000, res=600)
grid.arrange(table2, newpage = TRUE)
dev.off() #Print table
  
########################  

lisa.test <- localmoran(income.pm25$Income, van.lw, alternative = "two.sided")

income.pm25$Ii <- lisa.test[,1]
income.pm25$E.Ii<- lisa.test[,2]
income.pm25$Var.Ii<- lisa.test[,3]
income.pm25$Z.Ii<- lisa.test[,4]
income.pm25$P<- lisa.test[,5]
View(income.pm25@data)
########################

map_LISA <- tm_shape(income.pm25) + 
  tm_polygons(col = "Ii", 
              title = "Median Income\n Local Moran's I", 
              style = "fisher", 
              palette = "-RdBu", n = 10, midpoint = NA, lwd = 0.5) 
map_LISA

map_LISA_P <- tm_shape(income.pm25) + 
  tm_polygons(col = "P", 
              title = "Median Income\n P Values of Local Moran's I", 
              style = "fixed", 
              breaks = c(0,0.01,0.05,0.10,1),
              palette = "Reds", n = 1, lwd = 0.5) 

map_LISA_P

map_LISA_Z <- tm_shape(income.pm25) + 
  tm_polygons(col = "Z.Ii", 
              title = "Z Score", 
              style = "fixed",
              breaks = c(-11.39,-1.96,1.96,20),
              palette = "-RdBu", n = 10, lwd = 0.2,midpoint = 0)+
  tm_legend(legend.outside = FALSE,legend.position = c(0.03,0.11), legend.title.size = 0.75,legend.text.size = 0.5)+
  #tm_layout(frame = TRUE, main.title = "Local Moran's I Z-Test Scores for Median Income in Greater Vancouver", main.title.size = 0.6, main.title.position = 0.025)+
  tm_scale_bar(width = 0.16, position = c("LEFT", "BOTTOM"))+
  tm_compass( position = c("RIGHT", "TOP"))

map_LISA_Z

########################

moran.plot(income.pm25$Income, van.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Median Income", 
           ylab="Spatially Lagged Median Income", quiet=NULL)

########################

# TREND SURFACE ANALYSIS --------------------------------------------------
# Define the 1st order polynomial equation

f.1 <- as.formula(PM25AGG ~ X + Y) 

# Add X and Y to P
spSample$X <- coordinates(spSample)[,1]
spSample$Y <- coordinates(spSample)[,2]

# Run the regression model
lm.1 <- lm( f.1, data=spSample)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Southern California
r   <- raster(dat.1st)
r.m <- mask(r, income.pm25)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="-RdBu", 
            title="Predicted PM2.5") +
  tm_shape(spSample) + 
  tm_dots(col="PM25AGG", palette = "-RdBu", 
          title="Sampled PM2.5", size = 0.1)+
  tm_legend(legend.outside=TRUE)


# Define the 2nd order polynomial equation (can go up to 12 orders? think high school math)
# I function is for R to stay organized
f.2 <- as.formula(PM25AGG ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Add X and Y to P
spSample$X <- coordinates(spSample)[,1]
spSample$Y <- coordinates(spSample)[,2]

# Run the regression model
lm.2 <- lm( f.2, data=spSample)

# Use the regression model output to interpolate the surface
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.2nd)
r.m <- mask(r, income.pm25)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="-RdBu", 
            title="Predicted PM2.5") +
  tm_shape(spSample) + 
  tm_dots(col="PM25AGG", palette = "-RdBu", 
          title="Sampled PM2.5", size = 0.1)+
  tm_legend(legend.outside=TRUE)

########################

# UNIVERSAL KRIGING -------------------------------------------------------

f.1 <- as.formula(PM25AGG ~ X + Y) 
f.2 <- as.formula(PM25AGG ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))


var.smpl <- variogram(f.1, spSample, cloud = FALSE) #, cutoff=1000000, width=89900)
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(model="Gau"))

modelf.1 <- plot(var.smpl, dat.fit)
modelf.1

#f.2
var.smpl <- variogram(f.2, spSample, cloud = FALSE) #, cutoff=1000000, width=89900)
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=0.75, model="Sph"))

modelf.2 <- plot(var.smpl, dat.fit)
modelf.2

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige(f.2, spSample, grd, dat.fit)
View(dat.krg@data)
# Convert kriged surface to a raster object for clipping
dat.krg$var1.pred[dat.krg$var1.pred < 0] <- 0
r <- raster(dat.krg)
r.m <- mask(r, income.pm25)

# Plot the map
interpolatedPM2.5 <- tm_shape(income.tracts) +
  tm_polygons()+
tm_shape(r) + 
  tm_raster(n=10, palette="-RdBu",  
            title="Predicted PM2.5", midpoint = 0,alpha = 0.8) +
  tm_shape(spSample) + 
  tm_dots(col="PM25AGG", palette = "-RdBu", 
          title="Sampled PM2.5", size = 0.19, midpoint = 0)+
  tm_legend(legend.outside = FALSE,legend.position = c(0.03,0.005), legend.title.size = 0.48,legend.text.size = 0.45)+
  #tm_legend(legend.outside = TRUE, legend.title.size = 0.75)+
  #tm_layout(frame = TRUE, main.title = "Estimated Surface of PM2.5 Values from 390 Known Points Using Universal Kriging", main.title.size = 0.6, main.title.position = 0.05)+
  tm_scale_bar(width = 0.09, position = c("center", "top"))+
  tm_compass( position = c("RIGHT", "TOP"))

interpolatedPM2.5

# variance map (edge effects, no data along the edges so its hard to predict it)

r   <- raster(dat.krg, layer="var1.var")
r.m <- mask(r, income.pm25)

tm_shape(r) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map") +
  tm_shape(spSample) + 
  tm_dots(size=0.1) +
  tm_legend(legend.outside=TRUE)


#confidence interval map
r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- mask(r, income.pm25)

tm_shape(r) + 
  tm_raster(n=7, palette ="Reds",
            title="95% CI map \n(in ppm)") +
  tm_shape(spSample) + 
  tm_dots(size=0.1) +
  tm_legend(legend.outside=TRUE)


########################

# INTERPOLATION/POLYGON COMBINATION ---------------------------------------

step.5 <- extract(r, income.tracts, fun=mean, sp=TRUE)

#You are now ready to perform a regression

########################

# LINEAR REGRESSION -------------------------------------------------------
#Let's say your dataset with both PM2.5 and Income are stored in a dataset called pm.income.poly.
pm.income.poly <- step.5
names(pm.income.poly)[names(pm.income.poly) == "var1.pred"] <- "PM25"
sum(pm.income.poly$PM25, na.rm = TRUE)
# OMITS NA VALUES OF PM25 RESULTING FROM THE BOUNDS OF TH INTERPOLATED OUTPUT GRID NOT INCLUDING POINTS IN THOSE REGIONS
pm.income.poly <- pm.income.poly[!is.na(pm.income.poly$PM25),]
View(pm.income.poly@data)
sum(is.na(pm.income.poly$PM25))
pm.income.poly$PM25[pm.income.poly$PM25 < 0] <- 0

#map the PM25 values per DA
clippedPM2.5 <- tm_shape(pm.income.poly)+
  tm_fill(col = "PM25",title="Estimated\n PM2.5", palette = "Reds")+
  tm_borders(lwd=0.5)+
  tm_shape(spSample) + 
  tm_dots(col="PM25AGG", palette = "-RdBu", 
          title="Sampled\n PM2.5", size = 0.1, midpoint = 0)+
  tm_legend(legend.outside = FALSE,legend.position = c("LEFT","BOTTOM"), legend.title.size = 0.5,legend.text.size = 0.38)+
  #tm_layout(frame = TRUE, main.title = "Estimated Surface of PM2.5 Values Clipped to Underlying Dissemination Areas", main.title.size = 0.6, main.title.position = 0.05)+
  tm_scale_bar(width = 0.12, position = c("center", "BOTTOM"))+
  tm_compass( position = c("RIGHT", "TOP"))

clippedPM2.5

#Plot income and PM2.5 from the pm.income.poly dataset you created
plot(pm.income.poly$Income~pm.income.poly$PM25, xlab = "PM 2.5 Concentration (μg/m³)", ylab = "Median Income (CAD$)")

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(pm.income.poly$Income~pm.income.poly$PM25)

#Add the regression model to the plot you created
abline(lm.model, lwd = 4.0, col = "Red")

legend("topleft", bty="n", legend=paste(" R² =",format(summary(lm.model)$adj.r.squared, digits=4),"\n","P-Value = 1.027e-13"))
legend("topright", bty="n", legend=paste(" Residual Min: -24247", "\n", "Residual Median: -704\n", "Residual Max: 45109" ))

#Get the summary of the results
summary(lm.model)
View(lm.model)

#You want to determine if the model residuals are spatially clustered. 
#First obtain the residuals from the model
model.resids <- as.data.frame(residuals.lm(lm.model))
#Then add the residuals to your spatialpolygon dataframe
pm.income.poly$residuals <- residuals.lm(lm.model)
#Observe the result to make sure it looks correct
head(pm.income.poly)
View(pm.income.poly@data)
#Now, create choropleth map of residuals

tm_shape(pm.income.poly)+
  tm_polygons(col = "residuals", 
              title = "Residuals", 
              style = "fisher", 
              palette = "-RdBu",lwd = 0.5, midpoint= NA)

###############################

# GLOBAL MORANS I TO INVESTIGATE SPATIAL AUTOCORRELATION OF RESIDUALS --------

summary(pm.income.poly)

########################

PM25.nb <- poly2nb(pm.income.poly)
PM25.net <- nb2lines(PM25.nb,coords=coordinates(pm.income.poly))


tm_shape(pm.income.poly) + 
  tm_borders(col='lightgrey', lwd = 1) + 
  tm_shape(PM25.net) + 
  tm_lines(col='red', lwd = 0.5)

########################

PM25.lw <- nb2listw(PM25.nb, zero.policy = TRUE, style = "W")
print.listw(PM25.lw, zero.policy = TRUE)

########################


mi <- moran.test(pm.income.poly$residuals, PM25.lw, zero.policy = TRUE, alternative = "two.sided")
mi

moran.range(PM25.lw)

Morans.I <- mi$estimate[[1]]
Expected.I <- mi$estimate[[2]]
Variance <- mi$estimate[[3]]

Z.Score <- (Morans.I - Expected.I) / sqrt(Variance)

Morans.I <- round(Morans.I, digits = 3)

Expected.I <- round(Expected.I, digits = 5)

Variance <- round(Variance, digits = 5)

Z.Score <- round(Z.Score, digits = 3)

data.for.table3 = data.frame(Morans.I, Expected.I, Variance, Z.Score)


#Make table for Global Morans I
columns <- c("Moran's I", "Expected I", "Variance", "Z Score")
table3 <- tableGrob(data.for.table3) #make a table "Graphical Object" (GrOb) 
t3Caption <- textGrob("Table 3: Global Moran's I Results For OLS Residuals", gp = gpar(fontsize = 08))
padding <- unit(5, "mm")

table3 <- gtable_add_rows(table3, 
                          heights = grobHeight(t3Caption) + padding, 
                          pos = 0)

table3 <- gtable_add_grob(table3,
                          t3Caption, t = 1, l = 2, r = ncol(data.for.table3) +1)


grid.arrange(table3, newpage = TRUE)


#Printing a table 
png("Residuals_Global_Moran.png", pointsize=10, width=4000, height=2000, res=600)
grid.arrange(table3, newpage = TRUE)
dev.off() #Print table

########################

# GWR ---------------------------------------------------------------------
#Let's say you are continuing with your data from the regression analysis. 

#The first thing you need to do is to add the polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the "coordinates" function from the sp library
pm.income.poly <- spTransform(pm.income.poly, CRS(("+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83")))
pm.income.poly.coords <- sp::coordinates(pm.income.poly)
#Observe the result
head(pm.income.poly.coords)
#Now add the coordinates back to the spatialpolygondataframe
pm.income.poly$X <- pm.income.poly.coords[,1]
pm.income.poly$Y <- pm.income.poly.coords[,2]
head(pm.income.poly)

###Determine the bandwidth for GWR: this will take a while
GWRbandwidth <- gwr.sel(pm.income.poly$Income~pm.income.poly$PM25, 
                        data=pm.income.poly, coords=cbind(pm.income.poly$X,pm.income.poly$Y),adapt=T) 

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while
gwr.model = gwr(pm.income.poly$Income~pm.income.poly$PM25, 
                data=pm.income.poly, coords=cbind(pm.income.poly$X,pm.income.poly$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#Print the results of the model
gwr.model

#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
names(gwr.model$SDF)
head(results)
View(results)

#Now for the magic. Let's add our local r-square values to the map
pm.income.poly$localr <- results$localR2
pm.income.poly$coeff <- results$pm.income.poly.PM25
View(pm.income.poly@data)
pm.income.poly <- subset.data.frame(pm.income.poly,pm.income.poly$localr>=0)
other <- subset(pm.income.poly,pm.income.poly$localr>=0.35)  
View(other@data)
range(pm.income.poly$localr)
otherother <- subset(other,other$coeff>0)  
nrow(otherother)
range(pm.income.poly$coeff)
# #Create choropleth map of r-square values

localr <- tm_shape(pm.income.poly)+
  tm_polygons(col = "localr", 
              title = "Local R² Values", 
              style = "fixed", 
              breaks = c(0,0.2,0.4,0.6,0.8,1),
              palette = "-RdBu",lwd = 0.5, midpoint= NA)+
  tm_legend(legend.outside = TRUE, legend.title.size = 0.68)+
  tm_layout(frame = TRUE, main.title = "GWR Local R² per Dissemination Area in Greater Vancouver", main.title.size = 0.65, main.title.position = 0.05)+
  tm_scale_bar(width = 0.16, position = c("center", "BOTTOM"))+
  tm_compass( position = c("RIGHT", "TOP"))

localr

localr <- tm_shape(pm.income.poly)+
  tm_polygons(lwd = 0.2, alpha = 0.5)+
tm_shape(other)+
tm_polygons(col = "localr", 
            title = "Local R² Values", 
            style = "fixed", 
            breaks = c(0.35,0.4,0.5,0.6,0.7,0.8,0.9),
            palette = "-RdBu",lwd = 0.2, midpoint= NA, alpha = 0.5)+
  tm_text("localr", size = 0.5, legend.format = "2")+
  tm_legend(legend.outside = FALSE,legend.position = c(0.005,0.11), legend.title.size = 0.5,legend.text.size = 0.35)+
  #tm_layout(frame = TRUE, main.title = "GWR Local R² per Dissemination Area in Greater Vancouver", main.title.size = 0.65, main.title.position = 0.05)+
  tm_scale_bar(width = 0.09, position = c("LEFT", "BOTTOM"))+
  tm_compass( position = c("RIGHT", "TOP"))

#Time for more magic. Let's map the coefficients
#Create choropleth map of the coefficients

coefficient <- tm_shape(pm.income.poly)+
  tm_polygons(col = "coeff", 
              title = "Coefficient Value", 
              style = "fisher", 
              palette = "-RdBu",lwd = 0.2, midpoint= NA, n =10)+
  tm_legend(legend.outside = TRUE, legend.title.size = 0.68)+
  tm_layout(frame = TRUE, main.title = "GWR Coefficient per Dissemination Area in Greater Vancouver", main.title.size = 0.65, main.title.position = 0.02)+
  tm_scale_bar(width = 0.16, position = c("LEFT", "BOTTOM"))+
  tm_compass( position = c("RIGHT", "TOP"))
  
coefficient <- tm_shape(pm.income.poly)+
  tm_polygons(lwd = 0.2, alpha = 0.5)+
  tm_shape(other)+
  tm_polygons(col = "coeff", 
              title = "Coefficient Value", 
              style = "fisher", 
              palette = "-RdBu",lwd = 0.2, midpoint= NA, n =10, alpha = 0.5)+
  tm_legend(legend.outside = FALSE,legend.position = c(0.005,0.01), legend.title.size = 0.5,legend.text.size = 0.35)+
  #tm_layout(frame = TRUE, main.title = "GWR Coefficient per Dissemination Area in Greater Vancouver", main.title.size = 0.65, main.title.position = 0.02)+
  tm_scale_bar(width = 0.09, position = c("center", "BOTTOM"))+
  tm_compass( position = c("RIGHT", "TOP"))

coefficient

#####################

#POINT PATTERN ANALYSIS -------------------------------------------------

vancouver <- shapefile("./vancouver1")
vancouver <- spTransform(vancouver, CRS(("+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83")))

kma <- spSample

#PROJECT TO UTM ZONE 10N
kma <- spTransform(kma, CRS(("+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83")))

kma$x <- coordinates(kma)[,1]
kma$y <- coordinates(kma)[,2]

#check for and remove duplicated points
#check for duplicated points
#finds zero distance among points
zd <- zerodist(kma)
zd
#remove duplicates
kma <- remove.duplicates(kma)

#create an "extent" object which can be used to create the observation window for spatstat
kma.ext <- as.matrix(extent(kma)) 

#observation window
window <- as.owin(list(xrange = kma.ext[1,], yrange = kma.ext[2,]))

#create ppp oject from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)
class(kma.ppp)

########################

##Nearest Neighbour Distance

nearestNeighbour <- nndist(kma.ppp)

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))

##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"

##Calculate the nearest neighbor statistic to test for a random spatial distribution.
#mean nearest neighbour
nnd = mean(nearestNeighbour$Distance)

#mean nearest neighbour for random spatial distribution
Xmin <- kma.ext[1,1]
Xmax <- kma.ext[1,2]

Ymin <- kma.ext[2,1]
Ymax <- kma.ext[2,2]

studyarea <- (Xmax - Xmin) * (Ymax - Ymin) #length * width

N <- nrow(spSample)

pointDensity <- N/studyarea

r.nnd = 1/(2*(pointDensity**0.5))

d.nnd = 1.07453/(pointDensity**0.5)

R = nnd/r.nnd

SE.NND <- .26136/((N*pointDensity)**0.5)

z = (nnd - r.nnd)/SE.NND
  



Average.NND <- "745.435 m"

Random.NND <- "1201.606 m"

Z.Score <- round(z, digits = 3)

Point.Density <- round(pointDensity, digits = 3)

Study.Area.Size <- "2,252 km²"

data.for.table4 = data.frame(Study.Area.Size, Point.Density,Average.NND, Random.NND, Z.Score)


#Make table for NND

table4 <- tableGrob(data.for.table4) #make a table "Graphical Object" (GrOb) 
t4Caption <- textGrob("Table 4: Nearest Neighbour Distance Analysis Results on Known PM2.5 Sample Points", gp = gpar(fontsize = 08))
padding <- unit(5, "mm")

table4 <- gtable_add_rows(table4, 
                          heights = grobHeight(t4Caption) + padding, 
                          pos = 0)

table4 <- gtable_add_grob(table4,
                          t4Caption, t = 1, l = 2, r = ncol(data.for.table4) +1)


grid.arrange(table4, newpage = TRUE)


#Printing a table 
png("NND_results.png", pointsize=10, width=4000, height=2000, res=600)
grid.arrange(table4, newpage = TRUE)
dev.off() #Print table


# Descriptive Stats ------------------------------------------------------

range(pm.income.poly$Income) 
range(pm.income.poly$PM25)


#Mean
meanIncome <- mean(pm.income.poly$Income) #This is going to produce a wrong value (NA) due to a single NA value in data
meanIncome
meanPM25 <- mean(pm.income.poly$PM25)
meanPM25

#Standard Deviation
sdIncome <- sd(pm.income.poly$Income) #Calculate the SD, ignoring NA values
sdIncome
sdPM25 <- sd(pm.income.poly$PM25) #Calculate the SD, ignoring NA values only for the summer months
sdPM25

#Median
medIncome <- median(pm.income.poly$Income)
medIncome
medPM25 <- median(pm.income.poly$PM25)
medPM25

#Skewness
skewIncome <- skewness(pm.income.poly$Income)[1]
skewIncome
skewPM25 <- skewness(pm.income.poly$PM25)[1]
skewPM25


#Normal distribution test
normIncome_PVAL <- shapiro.test(pm.income.poly$Income)$p.value
normIncome_PVAL
normPM2.5_PVAL <- shapiro.test(pm.income.poly$PM25)$p.value
normPM2.5_PVAL

meanIncome <- round(meanIncome, digits = 3)
meanPM25 <- round(meanPM25, digits = 3)

sdIncome <- round(sdIncome, digits = 3)
sdPM25 <- round(sdPM25, digits = 3)

medIncome <- round(medIncome, digits = 3)
medPM25 <- round(medPM25, digits = 3)

skewIncome <- round(skewIncome, digits = 3)
skewPM25 <- round(skewPM25, digits = 3)



# Figures and Tables ------------------------------------------------------

#####
#Create a table of descriptive stats

Samples = c("Median Income", "PM 2.5") #Create an object for the labels
Mean = c(meanIncome, meanPM25) #Create an object for the means
SD = c(sdIncome, sdPM25) #Create an object for the standard deviations
Median = c(medIncome, medPM25) #Create an object for the medians
Skewness <- c(skewIncome, skewPM25) #Create an object for the skewness

Normality <- c(normIncome_PVAL, normPM2.5_PVAL) #Create an object for the normality PVALUE

data.for.table1 = data.frame(Samples, Mean, SD, Median, Skewness, Normality)


#Make table 1
table1 <- tableGrob(data.for.table1, rows = c("","")) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Table 1: 2016 Greater Vancouver Descriptive Statistics For Median Income & PM 2.5", gp = gpar(fontsize = 08))
padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)




grid.arrange(table1, newpage = TRUE)


#Printing a table (You can use the same setup for printing other types of objects (see ?png))
png("Descriptive_Stats.png", pointsize=10, width=4000, height=2000, res=600)
grid.arrange(table1, newpage = TRUE)
dev.off() #Print table

#####
tmap_save(tm = map_mdincome, filename = "MedianIncome_in_Van.png", width = 3000, height = 2000, dpi = 600)
tmap_save(tm = map_LISA_Z, filename = "Z_score_map_med_income.png", width = 3000, height = 2000, dpi = 600)
tmap_save(tm = interpolatedPM2.5, filename = "interpolatedPM2.5.png", width = 3000, height = 2000, dpi = 600)
tmap_save(tm = clippedPM2.5, filename = "clippedPM2.5.png", width = 3000, height = 2000, dpi = 600)
tmap_save(tm = localr, filename = "mapped_localr.png", width = 3000, height = 2000, dpi = 600)
tmap_save(tm = coefficient, filename = "mapped_coeff.png", width = 3000, height = 2000, dpi = 600)
tmap_save(tm = vancouver, filename = "vancouver.png", width = 3000, height = 2000, dpi = 600)

tmap_mode("view")
tmap_mode("plot")
localr
coefficient
#study site map
#good start
vancouver <- tm_shape(income.tracts) + 
  tm_polygons() +
  tm_shape(income.tracts) +
  tm_polygons(alpha = 0.3,) +
  #tm_layout(frame = TRUE, main.title = "Greater Vancouver, British Columbia Dissemination Areas", main.title.size = 0.6, main.title.position = 0.19)+
  tm_scale_bar(width = 0.16, position = c("LEFT", "BOTTOM"))+
  tm_compass( position = c("RIGHT", "TOP"))
