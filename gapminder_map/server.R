###############################################################################
######################### World Development Shiny App #########################
###############################################################################

###############################################################################
# Load in packages
library(shiny)
library(WDI)
library(DT)
#library(rsconnect)
#rsconnect::setAccountInfo(name='kayleehodgson',
#                          token='13DE338C70DD0EC5D2E1DEEB4B873F0F',
#                          secret='8qkC+mrgfcEQn7pQrWXCTc7tVd/Itcl4JKbjnmZA')
#rsconnect::deployApp()

###############################################################################
# SERVER FUNCTION

function(input, output) {
  
  output$title2 <- renderUI({
    HTML(paste("World Development Trends Over Time","(1990-2016)",sep="<br/>"))
  })
  
  output$plotui <- renderUI({
    plotOutput("ScatterPlot", height=420,
               click = "ScatterPlot_click",
               hover = hoverOpts(
                 id = "ScatterPlot_hover"))
  })
  
  output$ScatterPlot <- renderPlot({
    
    # subset data by year chosen
    mydata <- mydata[mydata[,"year"]==input$year,]
    
    # assign the log vs. linear functions
    logscale <- input$logscale
    if(logscale=="bothlinear"){
      xlogscale <- "Linear"
      ylogscale <- "Linear"
    } else if(logscale=="logx"){
      xlogscale <- "Log"
      ylogscale <- "Linear"
    } else if(logscale=="logy"){
      xlogscale <- "Linear"
      ylogscale <- "Log"
    } else if(logscale=="bothlog"){
      xlogscale <- "Log"
      ylogscale <- "Log"
    }
    
    # change the y axis labels based on variable chosen
    YName <- ifelse(input$variable=="SP.DYN.TFRT.IN","Babies per woman",
                    ifelse(input$variable=="EN.ATM.CO2E.PC","CO2 Emissions",
                           ifelse(input$variable=="SP.DYN.LE00.IN","Life Expectancy",
                                  ifelse(input$variable=="SH.DYN.MORT","Child Mortality"))))
    YLabs <- ifelse(input$variable=="SP.DYN.TFRT.IN","total fertility",
                    ifelse(input$variable=="EN.ATM.CO2E.PC","tonnes per person",
                           ifelse(input$variable=="SP.DYN.LE00.IN","year",
                                  ifelse(input$variable=="SH.DYN.MORT","0-5 year-olds dying per 1000 live births"))))

    # specify the x values and labels for axis
    xaxtvals <- c(500,1000,2000,4000,8000,16000,32000,64000,128000)
    xaxtlabs <- c('500','1000','2000','4000','8000','16k','32k','64k','128k')
    
    # specify the y values and labels for axis
    if(input$variable=="SP.DYN.TFRT.IN") { yaxtlabs <- seq(2,8,by=1)
    } else if(input$variable=="EN.ATM.CO2E.PC") { yaxtlabs <- seq(0,100,by=20)
    } else if(input$variable=="SP.DYN.LE00.IN") { yaxtlabs <- seq(10,80,by=10)
    } else if(input$variable=="SH.DYN.MORT") { yaxtlabs <- c(10,20,40,80,160,320) }
    yaxtvals <- yaxtlabs
    
    # specify the y and x limits
    if(input$variable=="SP.DYN.TFRT.IN") { ylimits <- c(0.1,9)
    } else if(input$variable=="EN.ATM.CO2E.PC") { ylimits <- c(0.1,101)
    } else if(input$variable=="SP.DYN.LE00.IN") { ylimits <- c(0.1,90)
    } else if(input$variable=="SH.DYN.MORT") { ylimits <- c(0.1,450) }
    ylimits <- ylimits
    xlimits <- c(180,130000)
    
    # Colors based on region
    speccol <- c("greenyellow","turquoise2","gold","brown1")
    outlinecol <- c("lightseagreen","turquoise4","gold4","brown3")
    
    # specify location of year text
    if(ylogscale=="Linear"){
      middle <- (YdataType(ylimits[1],ylogscale)+YdataType(ylimits[2],ylogscale))/2
    } else if(ylogscale=="Log"){
      middle <- exp((YdataType(ylimits[1],ylogscale)+YdataType(ylimits[2],ylogscale))/2)
    }
    if(xlogscale=="Linear"){
      middlex <- (XdataType(100,xlogscale)+XdataType(130000,xlogscale))/1.9
    } else if(xlogscale=="Log"){
      middlex <- exp((XdataType(100,xlogscale)+XdataType(130000,xlogscale))/1.9)
    }
    
    # create the plot
    par(mgp = c(1, 1, 0),mar=c(7, 4, 4, 2) + 0.1)
    switch(input$logscale,
           bothlinear = plot(mydata[,"NY.GDP.PCAP.PP.CD"], mydata[,input$variable],type="n",
                             xlim=xlimits, ylim=ylimits,bty='l',fg=colors()[300],ylab="",xlab="",
                             xaxt='n',yaxt='n'),
           logx = plot(mydata[,"NY.GDP.PCAP.PP.CD"], mydata[,input$variable],type="n",
                       xlim=xlimits, ylim=ylimits,bty='l',fg=colors()[300],ylab="",xlab="",
                       xaxt='n',yaxt='n',log="x"),
           logy = plot(mydata[,"NY.GDP.PCAP.PP.CD"], mydata[,input$variable],type="n",
                       xlim=xlimits, ylim=ylimits,bty='l',fg=colors()[300],ylab="",xlab="",
                       xaxt='n',yaxt='n',log="y"),
           bothlog = plot(mydata[,"NY.GDP.PCAP.PP.CD"], mydata[,input$variable],type="n",
                          xlim=xlimits, ylim=ylimits,bty='l',fg=colors()[300],ylab="",xlab="",
                          xaxt='n',yaxt='n',log="xy")
    )
    
    mtext(text=paste(YName,"\n","(",YLabs,")"),side=2,line=2)
    mtext(text=paste('Income','\n','(per person (GDP/capita, PPP$ inflation-adjusted))'),side=1,line=3)
    axis(side=1,pos=ylimits[1],at=xaxtvals,labels=xaxtlabs,tick=FALSE, col.axis=colors()[260],cex.axis=.65)
    axis(side=2,pos=150,at=yaxtvals,labels=yaxtlabs, col.axis=colors()[260],tick=FALSE,cex.axis=.65,las=1) # y axis
    text(middlex,middle,cex=10,input$year,col=colors()[140])
    abline(v=xaxtvals,col=colors()[300],lty=3,lwd=.5)
    abline(h=yaxtvals,col=colors()[300],lty=3,lwd=.5)
    points(mydata[,"NY.GDP.PCAP.PP.CD"], mydata[,input$variable], cex=(mydata[,"SP.POP.TOTL"]/100000000+1.5)/2,pch=21,lwd=1,
           bg=speccol[mydata[,"region"]],col=outlinecol[mydata[,"region"]])
  })

  # Click Output
  activeCountry <- reactiveVal()
  activePop <- reactiveVal()
  activeGDP <- reactiveVal()
  activeY <- reactiveVal()
  observeEvent(input$ScatterPlot_click, 
               {
                 nearCountry <- nearPoints(mydata,input$ScatterPlot_click,
                                           xvar="NY.GDP.PCAP.PP.CD", yvar=input$variable, threshold = 2,maxpoints=1)
                 activeCountry(as.character(nearCountry$country)) 
               })
  observeEvent(input$ScatterPlot_click, 
               {
                 nearCountry <- nearPoints(mydata,input$ScatterPlot_click,
                                           xvar="NY.GDP.PCAP.PP.CD", yvar=input$variable, threshold = 2,maxpoints=1)
                 activePop(as.character(nearCountry$SP.POP.TOTL)) 
               })
  observeEvent(input$ScatterPlot_click, 
               {
                 nearCountry <- nearPoints(mydata,input$ScatterPlot_click,
                                           xvar="NY.GDP.PCAP.PP.CD", yvar=input$variable, threshold = 2,maxpoints=1)
                 activeGDP(as.character(round(nearCountry$NY.GDP.PCAP.PP.CD,2))) 
               })
  observeEvent(input$ScatterPlot_click, 
               {
                 nearCountry <- nearPoints(mydata,input$ScatterPlot_click,
                                           xvar="NY.GDP.PCAP.PP.CD", yvar=input$variable, threshold = 2,maxpoints=1)
                 activeY(as.character(round(nearCountry[,input$variable],2))) 
               })
  activeCountry <- reactiveVal(value = "")
  activePop <- reactiveVal(value = " ")
  activeGDP <- reactiveVal(value = " ")
  activeY <- reactiveVal(value = " ")
  # Output for click function
  output$click_info <- renderUI({
    YName <- ifelse(input$variable=="SP.DYN.TFRT.IN","Babies per woman",
                    ifelse(input$variable=="EN.ATM.CO2E.PC","CO2 Emissions",
                           ifelse(input$variable=="SP.DYN.LE00.IN","Life Expectancy",
                                  ifelse(input$variable=="SH.DYN.MORT","Child Mortality"))))
    pop <- paste("Population: ",activePop())
    gdp <- paste("Income (GDP/Capita): ",activeGDP())
    y <- paste(YName,": ",activeY())
    HTML(paste(activeCountry(),pop,gdp,y,sep='<br/>'))
  })

  # Click Output
  activeCountryH <- reactiveVal()
  activePopH <- reactiveVal()
  activeGDPH <- reactiveVal()
  activeYH <- reactiveVal()
  observeEvent(input$ScatterPlot_hover, 
               {
                 nearCountryH <- nearPoints(mydata,input$ScatterPlot_hover,
                                           xvar="NY.GDP.PCAP.PP.CD", yvar=input$variable, threshold = 2,maxpoints=1)
                 activeCountryH(as.character(nearCountryH$country)) 
               })
  observeEvent(input$ScatterPlot_hover, 
               {
                 nearCountryH <- nearPoints(mydata,input$ScatterPlot_hover,
                                           xvar="NY.GDP.PCAP.PP.CD", yvar=input$variable, threshold = 2,maxpoints=1)
                 activePopH(as.character(nearCountryH$SP.POP.TOTL)) 
               })
  observeEvent(input$ScatterPlot_hover, 
               {
                 nearCountryH <- nearPoints(mydata,input$ScatterPlot_hover,
                                           xvar="NY.GDP.PCAP.PP.CD", yvar=input$variable, threshold = 2,maxpoints=1)
                 activeGDPH(as.character(round(nearCountryH$NY.GDP.PCAP.PP.CD,2))) 
               })
  observeEvent(input$ScatterPlot_hover, 
               {
                 nearCountryH <- nearPoints(mydata,input$ScatterPlot_hover,
                                           xvar="NY.GDP.PCAP.PP.CD", yvar=input$variable, threshold = 2,maxpoints=1)
                 activeYH(as.character(round(nearCountryH[,input$variable],2))) 
               })
  activeCountryH <- reactiveVal(value = "")
  activePopH <- reactiveVal(value = " ")
  activeGDPH <- reactiveVal(value = " ")
  activeYH <- reactiveVal(value = " ")
  
  # Output for hover function
  output$hover_info <- renderUI({
    YName <- ifelse(input$variable=="SP.DYN.TFRT.IN","Babies per woman",
                    ifelse(input$variable=="EN.ATM.CO2E.PC","CO2 Emissions",
                           ifelse(input$variable=="SP.DYN.LE00.IN","Life Expectancy",
                                  ifelse(input$variable=="SH.DYN.MORT","Child Mortality"))))
    pop <- paste("Population: ",activePopH())
    gdp <- paste("Income (GDP/Capita): ",activeGDPH())
    y <- paste(YName,": ",activeYH())
    HTML(paste(activeCountryH(),pop,gdp,y,sep='<br/>'))
  })
  
}

shinyApp(ui = ui, server = server)
deployApp()