library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
#import 2006-2016 data from each crms station and combine
filenames <- (c("E:/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_0355_Hourly.csv",
            "E:/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_0208_Hourly.csv",
            "E:/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_0164_Hourly.csv"))

read_csv_filename <- function(filename){
  ret <- read.csv(filename, sep = ",", header = T, strip.white = T,stringsAsFactors = F)
  ret$Source <- filename 
  ret
}

import.list <- ldply(filenames, read_csv_filename)
crms.ht <- Filter(function(x)!all(is.na(x)), import.list)

crms.ht$Date..mm.dd.yyyy. <-as.Date(crms.ht$Date..mm.dd.yyyy., format = "%m/%d/%Y")

crms.ht$datetime <- as.POSIXct(paste0(crms.ht$Date..mm.dd.yyyy., 
                                      crms.ht$Time..hh.mm.ss.), format = "%Y-%m-%d %H:%M:%S")

#head(melt(crms.ht, id.vars=c(datetime,Station.ID,))
#crms.ht$count <- as.integer(rownames(crms.ht))
crms.ht <- crms.ht[complete.cases(crms.ht$Adjusted.Water.Level..ft.),]
crms.ht$Station.ID <- substr(crms.ht$Station.ID,1,8)

ggplot(crms.ht, aes(datetime,Adjusted.Water.Level..ft.,group=Station.ID))+
  geom_line()+facet_wrap(~Station.ID,nrow = 3)

arrange(crms.ht,)
head(crms.ht[grep("0355",crms.ht$Station.ID),"datetime"])


