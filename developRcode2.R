#Reproduce Data Project 2

#reference packages
library(downloader)
library(R.utils)
library(data.table)
library(plyr)
library(ggplot2)

#set up options
#opts_chunk$set(fig.path='figure/')                #set path for figure output
#options("scipen"=999, "digits"=2)                 #modify options for sci notation and digits

#Set URL to download file and file name
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
filename <-paste(getwd(),"/repdata_data_StormData.csv.bz2",sep="")

#Download and unzip the file
download(fileUrl1, dest=filename, mode="wb") 
bunzip2("repdata_data_StormData.csv.bz2", remove = FALSE, overwrite = TRUE) 

#Read in the .csv file
rawdata <- read.csv("repdata_data_StormData.csv",
                    header=TRUE,
                    sep=",",
                    na.strings="NA",
                    stringsAsFactors=FALSE)

#list out variables to keep
keepvar<-c("BGN_DATE",
           "EVTYPE",
           "FATALITIES",
           "INJURIES",
           "PROPDMG",
           "PROPDMGEXP",
           "CROPDMG",
           "CROPDMGEXP")

#subset raw data to variables in keepvar
step1<-rawdata[,keepvar]
  

#Convert ENVTYPE to upper case calling it OLD
step1$EVTYPE<-toupper(step1$EVTYPE)
step1$PROPDMGEXP<-toupper(step1$PROPDMGEXP)
step1$CROPDMGEXP<-toupper(step1$CROPDMGEXP)


#Read in EVTYPE translation file
trans <- read.csv("EVTYPEtranslations2.csv",
                    header=TRUE,
                    sep=",",
                    na.strings="NA",
                    stringsAsFactors=FALSE)

#merge the EVTYPE in raw data with the translation file to get NEW VALUES
step2 <- merge(step1,trans,
                 by.x="EVTYPE",
                 by.y="OLD",
                 all.x=TRUE)

#read in translation file to conver PROPDMGEXP and CROPDMGEXP
moneytrans <- read.csv("moneytrans.csv",
                  header=TRUE,
                  sep=",",
                  na.strings="NA",
                  stringsAsFactors=FALSE)
moneytrans$prop<-moneytrans$FACTOR
moneytrans$crop<-moneytrans$FACTOR


#merge in translation file for PROPDMGEXP
step3 <- merge(step2,moneytrans[,c("FLAG","prop")],
                  by.x="PROPDMGEXP",
                  by.y="FLAG",
                  all.x=TRUE)

#merge in translation file for CROPDMGEXP
step4 <- merge(step3,moneytrans[,c("FLAG","crop")],
                   by.x="CROPDMGEXP",
                   by.y="FLAG",
                   all.x=TRUE)


#variable set up section for plots
step4$damage <- round(((step4$PROPDMG * step4$PROPDMG) + 
                         (step4$CROPDMG * step4$CROPDMG))/1000000,digits=0)
step4$people <- round((step4$FATALITIES * step4$INJURIES)/1000,digits=0)

step5<-ddply(step4,c("NEW"),
                     summarize,
                     damage=sum(damage,na.rm=TRUE),
                     people=sum(people,na.rm=TRUE)
             ) 

chart1<-head(step5[order(step5$people, decreasing=TRUE),],n=10)
p<-ggplot(chart1, aes(x = reorder(NEW,-people), y = people)) + geom_bar(stat = "identity") 
p<-p + geom_text(aes(label = people), size = 3, vjust=-.25, hjust=.5,)
p<-p + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1))
p

chart2<-head(step5[order(step5$damage, decreasing=TRUE),],n=10)
q<-ggplot(chart1, aes(x = reorder(NEW,-damage), y = damage)) + geom_bar(stat = "identity") 
q<-q + geom_text(aes(label = damage), size = 3, vjust=-.25, hjust=.5,)
q<-q + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1))
q
