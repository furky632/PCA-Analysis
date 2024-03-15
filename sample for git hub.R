#spatial analysis of Landsat ARD Data
#topic 1: Exploratory Data Analysis
#Mohammed Siddiquie

#step 1: Load Libraries
library(terra)
library(gdata)
library(corrplot)
library(sf)
library(pracma)
library(stats)
library(ggplot2)
library(cluster)
library(factoextra)

#step 2 : set working directory
path <- 'C:/Users/hp/Desktop/Desktop/QGIS/Newton CNTY/Mask layer'
setwd (path)

#step 3: Read all the bands
bands <- c('aerosolx','bluex','greenx','redx','NIRx','SWIR1x','SWIR2x')
Nbands <- length(bands)

#step 4: Read the newton vector file
fname <-'newton1.gpkg'
newton1 <- vect(fname)
plot(newton1)

#step 5: Read the raster and crop the data
for(i in seq(1,Nbands,1)){
  fname <- paste('band',i,'.tif',sep = "")
  x <- rast(fname)
  x <- crop(x,newton1)
  mv('x',bands[i])
}


#step 6: Create a raster stack with all data
newtonlsat <- c(aerosolx,bluex,greenx,redx,NIRx,SWIR1x,SWIR2x)

writeRaster(newtonlsat, "newtonlallbands.TIF",
            filetype = "Gtiff", overwrite = TRUE )
exists("newtonlsat")
#plot RBG and FCC Plots
#plot RGB Data Bands
plotRGB(newtonlsat,r=4, g=3, b=2,stretch ='hist')

#plot false color composite with NIR, red , green 
plotRGB(newtonlsat,r=5,g=4,b=3,stretch='hist')


#Step7: Create a Boxplot
bandnames <- c('aerosolx','bluex','greenx','redx','NIRx','SWIR1x','SWIR2x')
f <- boxplot(newtonlsat,axes=FALSE,outline=F,ylab='Value',notch=F)
axis(1,at=1:(Nbands),labels=bandnames,las=2)
axis(2)
title('DN for Various Bands in newton')
grid()
box()

# Write the bands as a dataframe for additional analysis
banddf <- as.data.frame(newtonlsat,xy=TRUE,)
head(banddf)
banddf <- na.omit(banddf)
colnames(banddf) <- c('X','Y',bandnames)
head(banddf)

# Compute correlation between bands
corbands <- cor(banddf[,3:9],method='spearman')
corrplot(corbands,method='number',type='lower',diag=FALSE)

#clamp values for constrant enhancement
hist (banddf[,3])
box()
LL<-0.05
UL<-0.95
bandnamec<-c()
for (i in seq(1,Nbands,1)){
  bname<-paste(bands[i],'c',sep="")
  xmin<-quantile(banddf[,(i+2)],LL,na.rm=T)
  xmax<-quantile(banddf[,(i+2)],UL,na.rm=T)
  x<- clamp(newtonlsat[[i]],lower=xmin,upper=xmax,val=TRUE)
  mv('x', bname)
}
library(stats)
pca_results<-prcomp(banddf[,3:9],center=TRUE , scale =TRUE)
pca_results
summary(pca_results)
