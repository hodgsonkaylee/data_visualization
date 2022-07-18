# load package
library(RColorBrewer)
library(ggmap)
library(WDI)
library(dplyr)
library(rworldmap)

# read in data that will be used to add region values
regiondat <- read.csv(file='totalfertilityandwealth.csv',header=T)
regiondat <- regiondat[,c(1,5)]
regiondat$region[regiondat$country=="Western Sahara"] <- 2
regiondat$region[regiondat$country=="Greenland"] <- 3
regiondat$region[regiondat$country=="French Guiana"] <- 1
write.csv(regiondat,file="regiondat.csv")

# load in the data using an API
#WDIsearch("GDP")
# GDP per capita, PPP (current international $) - "NY.GDP.PCAP.PP.CD"
#WDIsearch("Life")
# Life expectancy at birth, total (years) - "SP.DYN.LE00.IN"
#WDIsearch("population")
# Population, total - "SP.POP.TOTL" 
mydata <- WDI(country="all", 
              indicator=c("SP.DYN.LE00.IN","SP.POP.TOTL","NY.GDP.PCAP.PP.CD"),
              extra=TRUE, start=2016, end=2016)

head(mydata)

mydata <- subset(mydata,region!="Aggregates")

# change data names so that the merge includes all countries
mydata$country[mydata$country=="Russian Federation"] <- "Russia"
mydata$country[mydata$country=="Venezuela, RB"] <- "Venezuela"
mydata$country[mydata$country=="Egypt, Arab Rep."] <- "Egypt"
mydata$country[mydata$country=="Yemen, Rep."] <- "Yemen"
mydata$country[mydata$country=="Iran, Islamic Rep."] <- "Iran"
mydata$country[mydata$country=="Syrian Arab Republic"] <- "Syria"
mydata$country[mydata$country=="Lao PDR"] <- "Lao"
mydata$country[mydata$country=="Korea, Dem. People’s Rep."] <- "North Korea"
mydata$country[mydata$country=="Korea, Rep."] <- "South Korea"

# merge the region data from the other dataset with the API dataset
mydata <- merge(mydata,regiondat,by="country")
mydata[208,c(1,14)] <- c("French Guinea",1)
attach(mydata)
region.y <- as.factor(region.y)

# set color palette - fill and outline
speccol <- c("greenyellow","turquoise2","gold","brown1")
outlinecol <- c("lightseagreen","turquoise4","gold4","brown3")

# specify label values
xaxtvals <- c(500,1000,2000,4000,8000,16000,32000,64000,128000)
xaxtlabs <- c('$500','$1 000','$2 000','$4 000','$8 000','$16 000','$32 000','$64 000','$128 000')
yaxtvals <- seq(50,85,by=5)

# create the plot
pdf('gapminderlifeexpect.pdf',height = 8, width=11)
par(mgp = c(1, 1, 0))
plot(NULL,ylim=c(47.5,83.75), xlim=c(6.2,11.75), bty='o',
     fg=colors()[300], col.lab=colors()[300],
     ylab='Life Expectancy',xlab='GDP per Capita in International $',
     cex.lab=.65,xaxt='n',yaxt='n')

abline(h=c(46.2,85.05),lwd=3,col='lightblue') # light blue outlining - top/bottom
abline(v=c(5.992,11.96),lwd=3,col='lightblue') # light blue outlining - sides

axis(side=1,pos=47.5,at=log(xaxtvals),labels=xaxtlabs, col.axis=colors()[260],tick=FALSE,cex.axis=.65) # x axis
axis(side=2,pos=6.1,at=yaxtvals,labels=yaxtvals, col.axis=colors()[260],tick=FALSE,cex.axis=.65) # y axis
box()

text(log(1500),83,cex=2,"Gapminder World 2016",col='lightblue') # title
text(log(8000),47.5,cex=2.5,"INCOME",col=colors()[260]) # x axis label
text(log(3500),47.5,cex=1,"POOR",col=colors()[260]) # x axis directional labels
text(log(15500),47.5,cex=1,"RICH",col=colors()[260])
text(log(500),65.5,cex=2.5,"HEALTH",col=colors()[260],srt=90) # y axis label
text(log(475),73.5,cex=1,"HEALTHY",col=colors()[260],srt=90) # y axis directional labels
text(log(475),59,cex=1,"SICK",col=colors()[260],srt=90)

abline(v=log(xaxtvals),col=colors()[300],lty=3,lwd=.5) # grid lines - vertical
abline(h=yaxtvals,col=colors()[300],lty=3,lwd=.5) # grid lines - horizontal

points(log(NY.GDP.PCAP.PP.CD), SP.DYN.LE00.IN, cex=(SP.POP.TOTL/100000000+1.5),pch=21,lwd=.5,
       bg=speccol[region.y],col=outlinecol[region.y])
text(log(NY.GDP.PCAP.PP.CD), SP.DYN.LE00.IN, country, cex=sqrt(log(SP.POP.TOTL/100000000+1))+.15,lwd=2)

dev.off()

# create map with regions
WorldRegions <- joinCountryData2Map(mydata, joinCode = "ISO3", 
                                  nameJoinColumn = "iso3c",
                                  nameCountryColumn = "country")
speccol <- c("greenyellow","turquoise2","gold","brown1")

pdf("gapminderkey.pdf")
mapCountryData(WorldRegions, nameColumnToPlot = "region.y", catMethod = "categorical", lwd=.00000001,
               colourPalette = speccol, addLegend = FALSE, borderCol="white", missingCountryCol = "white",
               mapRegion = "world", mapTitle = "")
dev.off()

