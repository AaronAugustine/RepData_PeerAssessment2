#Reproduce Data Project 2

#reference packages used in the analysis
library(downloader)
library(R.utils)
library(plyr)
library(ggplot2)
library(utils)

#set up options
#opts_chunk$set(fig.path='figure/')                #set path for figure output


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
           "CROPDMGEXP",
           "REFNUM")

#subset raw data to variables in keepvar
step1<-rawdata[,keepvar]

#execute unique command to identify translations to build
unique(rawdata$EVTYPE)
unique(rawdata$PROPDMGEXP)
unique(rawdata$CROPDMGEXP)
  

#Read in EVTYPE translation file
trans <- read.csv("EVTYPEtranslations3.csv",
                    header=TRUE,
                    sep=",",
                    na.strings="NA",
                    stringsAsFactors=FALSE)

#merge the EVTYPE in raw data with the translation file to get NEW VALUES
step2 <- merge(step1,trans,
                 by.x="EVTYPE",
                 by.y="OLD",
                 all.x=TRUE)


#Convert PROPDMGEXP and CROPDMGEXP
tempPROPDMG <- mapvalues(step2$PROPDMGEXP, 
                         c("K","M","", "B","m","+","0","5","6","?","4","2","3","h","7","H","-","1","8"), 
                         c(1e3,1e6, 1, 1e9,1e6,  1,  1,1e5,1e6,  1,1e4,1e2,1e3,  1,1e7,1e2,  1, 10,1e8))

tempCROPDMG <- mapvalues(step2$CROPDMGEXP, 
                         c("","M","K","m","B","?","0","k","2"), 
                         c( 1,1e6,1e3,1e6,1e9,1,1,1e3,1e2))

step2$PROPTOTALDMG <- as.numeric(tempPROPDMG) * step2$PROPDMG
step2$CROPTOTALDMG <- as.numeric(tempCROPDMG) * step2$CROPDMG
step2$damage <- (step2$PROPTOTALDMG + step2$CROPTOTALDMG)/1000000000
step2$people <- (step2$FATALITIES + step2$INJURIES)/1000

step3<-ddply(step2,c("NEW"),
                     summarize,
                     damage=sum(damage,na.rm=TRUE),
                     people=sum(people,na.rm=TRUE)
             ) 

step3$damage<-round(step3$damage,digits=1)
step3$people<-round(step3$people,digits=1)

chart1<-head(step3[order(step3$people, decreasing=TRUE),],n=10)
p<-ggplot(chart1, aes(x = reorder(NEW,-people), y = people)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = people), size = 3, vjust=-.25, hjust=.5) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) +
    xlab("Event Type") + ylab("# of Fatalities & Injuries in 1000s") +
    ggtitle("Impact of Weather Events on population health")
p

chart2<-head(step3[order(step3$damage, decreasing=TRUE),],n=10) 
q<-ggplot(chart2, aes(x = reorder(NEW,-damage), y = damage)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = damage), size = 3, vjust=-.25, hjust=.5) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1)) +
    xlab("Event Type") + ylab("Property & Crop damage in billions") +
    ggtitle("Impact of Weather Events on Economy")
q
