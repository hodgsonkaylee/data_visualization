# load libraries
library(cartogram)
library(tmap)
library(maptools)
library(ggmap)
library(dplyr)
library(rworldmap)
library(RColorBrewer)

# Go to working directory and load the data
envdat <- read.csv(file="environment_data.csv",header=TRUE)
head(envdat)

# create combined variable clusters (based on exploratory factor analysis)

# Water and Environmental Wellbeing Cluster: water and sanitation, EPI, wastewater treatment, foundations of wellbeing
# Clean Air Cluster: indoor air pollution and greenhouse gases
# Biodiversity and Pest Regulation Cluster: biodiversity and pest regulation

envdat$Wellbeing <- scale(envdat$WaterSanitat) + scale(envdat$EnvPI) + scale(envdat$WastewaterTreat) + scale(envdat$FoundaWellbeing)
envdat$CleanAir <- scale(envdat$IndoorAirDeath16) + scale(envdat$GreenhseEmission)
envdat$CleanAirO <- ifelse(envdat$Country=="Central African Rep",NA,envdat$CleanAir)
envdat$BioPest <- scale(envdat$BiodiversHabitat) + scale(envdat$PestReg)

# 13, 21, 23, 24, 25, 26

# create two dataframes with "bad" and "good" Syndrome scores
envdat.high <- envdat[envdat$Syndrmtrichot=="High",]
envdat.middle <- envdat[envdat$Syndrmtrichot=="Middle",]
envdat.low <- envdat[envdat$Syndrmtrichot=="Low",]

#############################################################################
#### SYNDROME MAP ####

envdat$iso3 <- countrycode(envdat$Country,"country.name","iso3c")
envdat$iso3[32] <- "CAF"
envdat$iso3[45] <- "COD"

envdat$Syndrm2017Imp[which(envdat$Country=="Syria")] <- 13.0

envdat <- envdat[order(envdat$Syndrm2017Imp),]

MyWorldMap <- joinCountryData2Map(envdat, joinCode = "ISO3", 
                                  nameJoinColumn = "iso3",
                                  nameCountryColumn = "Country")
summary(MyWorldMap)

# create color palette
my.colors <- brewer.pal(8, 'Blues')
my.colors <- colorRampPalette(my.colors)(21)

# map the data and export
pdf("map.pdf")
mapCountryData(MyWorldMap, nameColumnToPlot = "Syndrm2017Imp", numCats=17,catMethod="categorical",
               colourPalette = my.colors[5:21], addLegend = TRUE, borderCol="white", missingCountryCol = "gray",
               mapRegion = "world", mapTitle = "The Syndrome Scale")
dev.off()

pdf("mapkey.pdf")
plot(envdat$Syndrm2017Imp,rep(0,176),cex=0.75,cex.axis=0.75,cex.lab=.75,pch=19,
     col=my.colors[3:20][as.factor(envdat$Syndrm2017Imp)],xlab="",ylab="",
     yaxt="n",xaxt="n",bty="n")
text(0:16,rep(.1,17),0:16,cex=.5)
dev.off()

#############################################################################
#### Scatterplots ####

colors <- c("red","turquoise","purple")

pdf("scatterplots.pdf",width=11,height=8)
par(mfrow=c(2,3))
plot(envdat$Syndrm2017Imp, envdat$Wellbeing, col=colors[envdat$Syndrmtrichot],
     cex.axis=0.75, cex=0.75, pch=19, ylim=c(-7,7.5),
     xlab="Subordination of Women", ylab="Water and Environmental Wellbeing Cluster"
     )
text(2,-6.8,"r=-.783**")
plot(envdat$Syndrm2017Imp, envdat$CleanAirO, col=colors[envdat$Syndrmtrichot],
     cex.axis=0.75, cex=0.75, pch=19, ylim=c(-2,3.5),
     xlab="Subordination of Women", ylab="Clean Air Cluster"
)
text(2,-1.8,"r=.604**")
plot(envdat$Syndrm2017Imp, envdat$BioPest, col=colors[envdat$Syndrmtrichot],
     cex.axis=0.75, cex=0.75, pch=19, ylim=c(-6,2),
     xlab="Subordination of Women", ylab="Biodiversity and Pest Regulation Cluster"
)
text(2,-5.8,"r=-.482**")
plot(envdat$Syndrm2017Imp, envdat$OutdoorAirDeath16, col=colors[envdat$Syndrmtrichot],
     cex.axis=0.75, cex=0.75, pch=19, ylim=c(-5,140),
     xlab="Subordination of Women", ylab="Deaths Caused by Outdoor Air Pollution"
)
text(2,-3,"r=.549**")
plot(envdat$Syndrm2017Imp, envdat$AirQual, col=colors[envdat$Syndrmtrichot],
     cex.axis=0.75, cex=0.75, pch=19, ylim=c(10,100),
     xlab="Subordination of Women", ylab="Air Quality"
)
text(2,12,"r=-.427**")
plot(envdat$Syndrm2017Imp, envdat$ClimateRiskIndex, col=colors[envdat$Syndrmtrichot],
     cex.axis=0.75, cex=0.75, pch=19, ylim=c(0,120),
     xlab="Subordination of Women", ylab="Global Climate Risk Index"
)
text(2,2,"r=.060")
dev.off()

#############################################################################
#### Bar charts #### 

SynMean <- apply(envdat[,c(24,27,26,13,21,23)],2,mean,na.rm=TRUE)
SynSD <- apply(envdat[,c(24,27,26,13,21,23)],2,sd,na.rm=TRUE)

highSynmean <- (colMeans(envdat.high[,c(24,27,26,13,21,23)],na.rm=TRUE)-SynMean)/SynSD
middleSynmean <- (colMeans(envdat.middle[,c(24,27,26,13,21,23)],na.rm=TRUE)-SynMean)/SynSD
lowSynmean <- (colMeans(envdat.low[,c(24,27,26,13,21,23)],na.rm=TRUE)-SynMean)/SynSD

pdf("barplot.pdf")
par(mfrow=c(1,1))
plot(1:6,rep(0,6),type="l",lwd=0, bty="n",
     ylim=c(-2,2),xaxt="n",yaxt="n",
     xlab="",ylab="",
     main="",xlim=c(1,6.5))
for(i in 1:length(lowSynmean)){
  segments(i,0,i,lowSynmean[i],lwd=8,col="turquoise")
}
for(i in 1:length(middleSynmean)){
  segments(i+.1,0,i+.1,middleSynmean[i],lwd=8,col="purple")
}
for(i in 1:length(highSynmean)){
  segments(i+.2,0,i+.2,highSynmean[i],lwd=8,col="red")
}
abline(h=0,lty=2)
dev.off()

#############################################################################
#### Coefficient Plots #### 

est.vals <- c(-.36,.08,-.205,2.088,-1.587,.721)
est.ci <- rbind(c(-2.233,3.676),c(-2.795,-.378),c(.355,3.82),
                c(-.337,-.073),c(.008,.152),c(-.518,-.202))
est.vals <- rev(est.vals)

pdf("coefficient.pdf",height=8,width=11)
plot(est.vals,1:6,cex.axis=0.75, cex=0.5, pch=19,
     xlab="Estimate", ylab="",xlim=c(-3,4),yaxt="n")
for(i in 1:length(est.vals)){
  segments(est.ci[i,1],i,est.ci[i,2],i,lwd=3,col="cornflowerblue")
}
axis(2,at=1:6,labels=rev(c("Water and Environmental Wellbeing Cluster",
                       "Clean Air Cluster",
                       "Biodiversity and Pest Regulation Cluster",
                       "Deaths Caused by Outdoor Air Pollution",
                       "Air Quality",
                       "Global Climate Risk Index")),las=1,cex=.3)
points(est.vals,1:6,cex=1,pch=19)
abline(v=0,lty=2)
dev.off()

#############################################################################
#### Proportional map #### 

library(countrycode)
library(maptools)
library(ggplot2)
library(rgeos)
library(rworldmap)

# get world map
wmap <- getMap(resolution="low")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data frame with centroids
df <- as.data.frame(centroids)
df$Country <- rownames(df)
head(df)

# combine the data
envdat$x <- envdat$y <- rep(NA,length(envdat$Country))
for(i in 1:length(envdat$Country)){
  for(j in 1:length(df$Country)){
    if(envdat$Country[i]==df$Country[j]){
      envdat$x[i] <- df$x[j]
      envdat$y[i] <- df$y[j]
    } 
  }
}
envdat$x[20] <- df$x[27]
envdat$y[20] <- df$y[27]
envdat$x[38] <- df$x[46]
envdat$y[38] <- df$y[46]
envdat$x[40] <- df$x[43]
envdat$y[40] <- df$y[43]
envdat$x[45] <- df$x[45]
envdat$y[45] <- df$y[45]
envdat$x[68] <- df$x[84]
envdat$y[68] <- df$y[84]
envdat$x[137] <- df$x[201]
envdat$y[137] <- df$y[201]
envdat$x[157] <- df$x[223]
envdat$y[157] <- df$y[223]
envdat$x[160] <- df$x[219]
envdat$y[160] <- df$y[219]
envdat$x[168] <- df$x[227]
envdat$y[168] <- df$y[227]

continents <- readShapeSpatial("continents/continent.shp")
pdf("pollutionmap.pdf")
plot(continents, col="darkolivegreen3", lty=0)
symbols(envdat$x, envdat$y, fg=NA, bg=my.colors[3:20][as.factor(envdat$Syndrm2017Imp)],
        circles=(2.7^envdat$AirPollution0), inches=FALSE, add=TRUE)
dev.off()

plot(continents, col="darkolivegreen3", lty=0)
symbols(envdat$x, envdat$y, fg=NA, bg=my.colors[3:20][as.factor(envdat$Syndrm2017Imp)],
        circles=(1.1^(-envdat$AirQual)*100), inches=FALSE, add=TRUE)



