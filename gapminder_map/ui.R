# Load in packages
library(shiny)
library(WDI)
library(DT)

###############################################################################
# UPLOAD AND CLEAN DATA

setwd("~/Desktop/Kaylee Work and School/Current Classes/POLI 301/Assignment 11")
regiondat <- read.csv("regiondat.csv",header=T)
mydata <- WDI(country="all", 
              indicator=c("SP.DYN.TFRT.IN","EN.ATM.CO2E.PC","SP.DYN.LE00.IN","SH.DYN.MORT","SP.POP.TOTL","NY.GDP.PCAP.PP.CD"),
              extra=TRUE, start=1990, end=2016)
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
mydata[208,c(1,18)] <- c("French Guinea",1)
mydata$region <- as.factor(mydata[,"region.y"])

XdataType <- function(x, type) {
  switch(type,
         Linear = c(x),
         Log = log(x))
}
YdataType <- function(x, type) {
  switch(type,
         Linear = c(x),
         Log = log(x))
}

###############################################################################
# USER INTERFACE FUNCTION

fluidPage(
  
  headerPanel(
    htmlOutput("title2")
  ),
  
  # Output scatterplot
  mainPanel(
    uiOutput("plotui"),
    h4("Individual Country Information:"),
    column(width=6,
           h5("(Click Points for Information)"),
           htmlOutput("click_info")),
    column(width=6,
           h5("(Hover Over Points for Information)"),
           htmlOutput("hover_info"))
  ),
  
  sidebarPanel(
    HTML('<center><img src="mapkey.png" width="250" height="150"></center>'),
    radioButtons("logscale", "Scale Type (Log vs. Linear)",
                 list("Both Linear"="bothlinear",
                      "Log X Axis"="logx",
                      "Log Y Axis"="logy",
                      "Both Log-Transformed"="bothlog"),selected="logx",inline=FALSE),
    selectInput("variable","Response Variable:",
                c("Total Fertility"="SP.DYN.TFRT.IN",
                  "CO2 Emissions"="EN.ATM.CO2E.PC",
                  "Life Expectancy"="SP.DYN.LE00.IN",
                  "Child Mortality"="SH.DYN.MORT")),
    sliderInput("year","Year",min=1990, max=2016, value=1,sep="",animate = TRUE),
    HTML('<center><img src="populationsize.png" width="160"></center>')
  )
)