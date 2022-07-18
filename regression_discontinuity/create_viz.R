library(WDI)
library(lattice)

# Upload the data
ideology <- read.csv(file="ideology.csv", header=TRUE)
ideology <- ideology[order(ideology$demvoteshare),]
ideology <- ideology[-which(is.na(ideology$demvoteshare)),]
ideology <- ideology[-which(is.na(ideology$realada)),]
head(ideology)

# Exclude when the vote share is 0 or 1, because that indicates that the 
# candidate ran unopposed
exclude <- c(which(ideology$demvoteshare==0),which(ideology$demvoteshare==1))
demvoteshare <- ideology$demvoteshare[-exclude]
realada <- ideology$realada[-exclude]

# set colors according to split
col <- ifelse(demvoteshare<.5,"red","blue")

# predict values for regression lines
pred.rep <- predict(loess(realada[demvoteshare<.5] ~ demvoteshare[demvoteshare<.5]))
pred.dem <- predict(loess(realada[demvoteshare>.5] ~ demvoteshare[demvoteshare>.5]))

# Measures of discontinuity
disc.min <- max(pred.rep)
disc.max <- min(pred.dem)

# PLOT
# x axis: vote shares for democratic candidate, y axis: ADA scores
pdf("ADAVoteShare.pdf")
plot(demvoteshare, realada,
     ylab='ADA Score',xlab='Vote Shares for Democratic Candidate',
     cex.lab=.65,xaxt='n',yaxt='n',
     xlim=c(-.01,1.01),
     cex=.7,pch=20,col=col)
abline(v=.5,lty=2)

axis(side=1,pos=-28,at=c(0,.2,.4,.6,.8,1),labels=c("0%","20%","40%","60%","80%","100%"),tick=FALSE,cex.axis=.65) # x axis
axis(side=2,pos=-.025,at=c(-30,0,30,60,90,120),labels=c(-30,0,30,60,90,120),tick=FALSE,cex.axis=.65,las=1) # y axis

lines(demvoteshare[demvoteshare<.5], pred.rep,lwd=2)
lines(demvoteshare[demvoteshare>.5], pred.dem,lwd=2)

segments(.5,disc.min,.5,disc.max,col="green",lwd=3)
text(.7,40,paste("Local treatment", paste("effect:", round(disc.max-disc.min, digits=2)),sep="\n"),
     cex=1, col="green")
dev.off()
