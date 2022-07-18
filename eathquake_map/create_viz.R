# load packages
library(RColorBrewer)
library(ggmap)
library(WDI)
library(dplyr)
library(rworldmap)
library(maps)
library(maptools)
library(mapdata)
library(lubridate)
library(openair)
library(lattice)
library(googleVis)

# read in data
eqdat <- read.csv(file='earthquakes.csv',header=T)
head(eqdat)

# create new variable with just the dates
eqdat$date <- ymd(as.character(substr(eqdat$time, start=1, stop=10)))

# subset the data
eqdat.w1 <- selectByDate(eqdat, start = "26/9/2018", end = "2/10/2018")
eqdat.w2 <- selectByDate(eqdat, start = "3/10/2018", end = "9/10/2018")
eqdat.w3 <- selectByDate(eqdat, start = "10/10/2018", end = "16/10/2018")
eqdat.w4 <- selectByDate(eqdat, start = "17/10/2018", end = "23/10/2018")

# find the two largest earthquakes for each week
w1max <- head(eqdat.w1[order(-eqdat.w1$mag),][,c(3,4,6,15)],2)
w2max <- head(eqdat.w2[order(-eqdat.w2$mag),][,c(3,4,6,15)],2)
w3max <- head(eqdat.w3[order(-eqdat.w3$mag),][,c(3,4,6,15)],2)
w4max <- head(eqdat.w4[order(-eqdat.w4$mag),][,c(3,4,6,15)],2)

# upload the world map (downloaded from site Baumoeller refers to)
continents <- readShapeSpatial("continents/continent.shp")

# map the earthquake for each of the 4 weeks
pdf("eqmaps.pdf")
par(mfrow=c(2,2))
par(mai=c(0,0,.2,0),xaxs="i",yaxs="i")

plot(continents, col="grey90", lty=0)
symbols(eqdat.w1$longitude, eqdat.w1$latitude, fg=NA, bg="#FF000022",
        circles=2.7^(eqdat.w1$mag), inches=0.5, add=TRUE)
text(w1max$longitude, w1max$latitude, w1max$mag)

plot(continents, col="grey90", lty=0)
symbols(eqdat.w2$longitude, eqdat.w2$latitude, fg=NA, bg="#FF000022",
        circles=2.7^(eqdat.w2$mag), inches=0.5, add=TRUE)
text(w2max$longitude, w2max$latitude, w2max$mag)

plot(continents, col="grey90", lty=0)
symbols(eqdat.w3$longitude, eqdat.w3$latitude, fg=NA, bg="#FF000022",
        circles=2.7^(eqdat.w3$mag), inches=0.5, add=TRUE)
text(w3max$longitude, w3max$latitude, w3max$mag)

plot(continents, col="grey90", lty=0)
symbols(eqdat.w4$longitude, eqdat.w4$latitude, fg=NA, bg="#FF000022",
        circles=2.7^(eqdat.w4$mag), inches=0.5, add=TRUE)
text(w4max$longitude, w4max$latitude, w4max$mag)

dev.off()

