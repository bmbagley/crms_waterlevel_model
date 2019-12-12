library(plyr)
library(tidyverse)
library(reshape2)
library(zoo)
############IMport###############
#import 2006-2016 data from each crms station and combine
filenames <- (c("E:/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_0355_Hourly.csv",
                "E:/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_0164_Hourly.csv",
                "E:/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_4529_Hourly.csv"))
filenames <- (c("C:/Users/Brandon/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_0355_HOURLY.csv",
                "C:/Users/Brandon/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_0164_HOURLY.csv",
                "C:/Users/Brandon/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_4529_HOURLY.csv"))


read_csv_filename <- function(filename){
  ret <- read.csv(filename, header = T, strip.white = T,stringsAsFactors = F)
  ret$Source <- filename 
  ret
}

import.list <- ldply(filenames, read_csv_filename)
crms.ht <- Filter(function(x)!all(is.na(x)), import.list)

crms.ht$Date..mm.dd.yyyy. <-as.Date(crms.ht$Date..mm.dd.yyyy., format = "%m/%d/%Y")

crms.ht$datetime <- as.POSIXct(paste0(crms.ht$Date..mm.dd.yyyy., 
                                      crms.ht$Time..hh.mm.ss.), format = "%Y-%m-%d %H:%M:%S")

crms.ht <- crms.ht[complete.cases(crms.ht$Adjusted.Water.Level..ft.),]
crms.ht$Station.ID <- substr(crms.ht$Station.ID,1,8)
crms.ht <- subset(crms.ht,datetime >= as.POSIXct("2006-07-01") & 
                    crms.ht$datetime<= as.POSIXct('2016-10-17 12:00'))

crms.ht <- arrange(crms.ht,Station.ID,datetime)

crms.ht$Adjusted.Water.ele.datum..ft. <- with(crms.ht, ifelse(Station.ID=="CRMS0164" & Geoid=="GEOID99", 
                                 Adjusted.Water.Elevation.to.Datum..ft.-0.73,
              ifelse(Station.ID=="CRMS0355" & Geoid == "GEOID99", 
                     Adjusted.Water.Elevation.to.Datum..ft.-0.9,
                ifelse(Station.ID=="CRMS4529" & Geoid == "GEOID99", 
                       Adjusted.Water.Elevation.to.Datum..ft.-0.86,
                       Adjusted.Water.Elevation.to.Datum..ft.))))
crms.ht$Adjusted.Water.ele.datum..cm. <- crms.ht$Adjusted.Water.ele.datum..ft.*30.48

rm("filenames","import.list","read_csv_filename")

ggplot(crms.ht, aes(datetime,Adjusted.Water.Level..ft.,group=Station.ID))+
  geom_line()+facet_wrap(~Station.ID,nrow = 3)+theme_light()+
  labs(y="Adjusted Water Level(ft)",x="",title="Adjusted Water Level throughout Louisiana Marshes \nJuly 2006 - October 2016")

#########Monthly water lvl full-PLOT###########
crms.ht$datetime2 <- as.POSIXlt(crms.ht$datetime)
crms.ht$day <- crms.ht$datetime2$mday
crms.ht$month <- crms.ht$datetime2$mon+1
crms.ht$year <- crms.ht$datetime2$year+1900
crms.ht$month <- factor(crms.ht$month, levels = c(seq(1,12,1)),
                        labels=c(month.name))
crms.ht$datetime2 <- NULL

a <- melt(crms.ht, id=c("Station.ID","day", "month","year"),measure.vars = "Adjusted.Water.ele.datum..cm.")

a <- ddply(a,.(Station.ID,month,year),summarize,
           mean=mean(value))
a$date <- as.POSIXct(as.yearmon(paste0(as.numeric(a$month),"-",a$year),format = "%m-%Y"))
a$Station.ID <- factor(a$Station.ID, levels = c("CRMS0164", "CRMS0355","CRMS4529"),
                       labels = c("CRMS4529-PS","CRMS0164-GI", 
                                  "CRMS0355-TB"))

ggplot(a, aes(date,mean,group=Station.ID, color=Station.ID))+
  geom_line(aes(group=Station.ID),size=1)+
  #facet_wrap(~Station.ID,ncol=1,scales = "free_x")+
  scale_x_datetime(date_labels = "%m-%Y",date_breaks= "4 month")+
  theme_light()+
  labs(title="Time Series of Daily Mean Sea Level\n2006-2016 from CRMS Stations",
       y="Water Level (cm)",x="")+
  theme(plot.title = element_text(size=18,face="bold"),
        panel.border = element_rect(color="black",size = 2),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=14,color = "black",angle = 45,hjust = 1),
        legend.position="none",
        #legend.title = element_text(size=16, face="bold.italic"),legend.text = element_text(size=12),
        #legend.position = c(.85,.85),legend.background = element_rect(size=1,color="black"),
        strip.text.x = element_text(size=14,color="black", face = "bold.italic"),
        strip.text.y = element_text(size=14,color="black", face = "bold.italic",
                                    angle = 0))#+
  geom_smooth(method="lm",aes(group=Station.ID),se=F)
########Avg Monthly Water Level###################
a <- crms.ht[,c("Station.ID","day", "month","year","Adjusted.Water.ele.datum..cm.")]
colnames(a)[5] <- "watlvl"
#a$watlvl <- a$watlvl*30.48

a <- melt(a, id=c("Station.ID","day", "month","year"),measure.vars = "watlvl")

a <- ddply(a,.(Station.ID,day,month),summarize,
           N=length(value),
           sd=sd(value),
           se=sd/sqrt(N),
           mean=mean(value))
a$date <- as.POSIXct(paste0(a$month,"-",a$day),format = "%B-%d")
a$Station.ID <- factor(a$Station.ID, levels = c("CRMS0164", "CRMS0355","CRMS4529"),
                       labels = c("CRMS4529-Port Sulphur","CRMS0164-Grand Isle", 
                                  "CRMS0355-Terrebonne Bay"))
#
ggplot(a, aes(date,mean,group=Station.ID,color=Station.ID))+
  #facet_wrap(~Station.ID, ncol=1)+
  geom_line(aes(group=Station.ID),size=1)+
  scale_x_datetime(date_labels = "%B",date_breaks= "1 month")+
  theme_light()+
  labs(title="Time Series of Daily Mean Sea Level\n2006-2016 from CRMS Stations",
       y="Sea Level Relative to NAVD88 Datum (cm)",x="")+
  theme(plot.title = element_text(size=18,face="bold"),
        panel.border = element_rect(color="black",size = 2),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=14,color = "black",angle = 45,hjust = 1),
        legend.title = element_text(size=16, face="bold.italic"),
        legend.text = element_text(size=12),
        legend.position = c(.5,.2),
        legend.background = element_rect(size=1,color="black"),
        strip.text.x = element_text(size=14,color="black", face = "bold.italic"))
  
#
ggsave("./Figures/Watlvl_DailyMean_CRMS.pdf",plot=last_plot(),scale=1,
       width=14,height=9)
###############Observed Data (CWC)#############################
#import water height over marsh observations
#waterht.obs <- read.csv("E:/Google Drive/UTK/GOMRI_Data/Env Data/BMB_Compiled/gom_collab_waterheight.csv",
#                        header = T,strip.white = T,stringsAsFactors = F)
waterht.obs <- read.csv("C:/Users/Brandon/Google Drive/UTK/GOMRI_Data/Env Data/BMB_Compiled/gom_collab_waterheight.csv",
                        header = T,strip.white = T,stringsAsFactors = F)
waterht.obs$END <- as.POSIXct(waterht.obs$END, format = "%m,%d,%Y %H:%M")
waterht.obs$site <- substr(waterht.obs$SAMPLE_SITE,1,2)

##############Grand Isle###############################
a <- crms.ht[grep("0164", crms.ht$Station.ID),] #gi
b <- crms.ht[grep("0164", crms.ht$Station.ID),]
b <- subset(b,datetime >= as.POSIXct("2012-01-01") & 
              b$datetime<= as.POSIXct('2015-01-01 00:00'))
nrow(a)/length(seq(as.POSIXct("2006-07-01 00:00"),as.POSIXct("2016-10-17 12:00"), by = "hours"))
#97.5% coverage overall
a$index <- index(a$datetime)
lm <- lm(a$Adjusted.Water.ele.datum..cm.~as.integer(a$datetime))
summary(lm)
#GI --- 4.187e-08 cm/sec = 1.32 cm/yr 
#r^2=0.056
lm.b <- lm(b$Adjusted.Water.ele.datum..cm.~as.integer(b$datetime))
summary(lm.b)
#long term wat lvl change = 2.300e-08 cm/sec ; 0.7252 cm/yr | 7.252 mm/yr
#r^2=0.002

###################Grand Isle - Marsh Elevation#####
a.1 <- crms_ele[grep("164",crms_ele$Station.ID),]
lm.ele <- lm(a.1$Ver.Pin.ht.cm~as.POSIXct(a.1$datetime))
#
a <- crms.ht[grep("164",crms.ht$Station.ID),]
a$datetime <- as.POSIXct(a$datetime)
a.obs <- filter(waterht.obs, site=="GI")
a.obs <- a.obs[,c("END", "INLAND_WAT_DEPTH_FT")]
a.obs$In_Wat_dep_cm <- a.obs$INLAND_WAT_DEPTH_FT*30.48
a.obs$watlvl <- a[match(a.obs$END,a$datetime),"Adjusted.Water.ele.datum..cm."]
a.obs$rel.marsh.ele.cm <- a.obs$watlvl-a.obs$In_Wat_dep_cm
a.obs$marsh.ele.inc.cm <- lm.ele$coefficients[2]*as.integer(a.obs$END)+lm.ele$coefficients[1]
a.obs <- arrange(a.obs, END)

a3 <- t.test(a.obs$marsh.ele.inc,a.obs$rel.marsh.ele.cm,paired = T)
lm.ele$coefficients[1]-a3$conf.int[2]

a$marsh.ele.cm <- (as.integer(a$datetime)*lm.ele$coefficients[2])+lm.ele$coefficients[1]-a3$conf.int[2]
a$adj.water.ele.to.marsh.cm <- a$Adjusted.Water.ele.datum..cm.-a$marsh.ele.cm
#
gi.crms.ht <- a[,c("Station.ID","datetime","marsh.ele.cm","adj.water.ele.to.marsh.cm")]

#######GI-fourier#######
t <- index(a$datetime)
ssp <- spectrum(a$Adjusted.Water.ele.datum..cm.)
per=1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(a$Adjusted.Water.ele.datum..cm.~sin(2*pi/per*t)+cos(2*pi/per*t)+
              sin(4*pi/per*t)+cos(4*pi/per*t))

#####GI-plot WL########
plot(a$Adjusted.Water.ele.datum..cm.~a$datetime, type="l",cex=.2,col="grey30",main="Grand Isle hourly water level,\n Average Water Level, CRMS Marsh Elevation, and Calculated Marsh Elevation \n CRMS Station #0164 from 2006-2016")
points(a$marsh.ele.cm~a$datetime, cex=.2, col="red")
points((a$Adjusted.Water.ele.datum..cm.-(a$Adjusted.Water.Elevation.to.Marsh..ft.*30.48))~a$datetime, col="green",cex=.2)
abline(lm(a$Adjusted.Water.ele.datum..cm.~a$datetime), cex=.2, col="blue")
#lines(fitted(reslm)~a$datetime, col="orange", lty=2)
## linear regression of corrected marsh elevation y= 2.558395e-08 X -22.05138
#################Terrebonne Bay##############################
a <- crms.ht[grep("0355", crms.ht$Station.ID),] #Tb
b <- crms.ht[grep("0355", crms.ht$Station.ID),]
b <- subset(b,datetime >= as.POSIXct("2012-01-01") & 
              b$datetime<= as.POSIXct('2015-01-01 00:00'))
nrow(a)/length(seq(as.POSIXct("2006-07-01 00:00"),as.POSIXct("2016-10-17 12:00"), by = "hours"))
#coverage = 98.4%
a$index <- index(a$datetime)
lm <- lm(a$Adjusted.Water.ele.datum..cm.~as.integer(a$datetime))
#wat lvl rise = -8.192e-09 cm/sec ; -0.258 cm/yr or -2.58mm/yr
#r^2=0.001626
lm.b <- lm(b$Adjusted.Water.ele.datum..cm.~as.integer(b$datetime))
#wat lvl rise = 7.423e-08 cm/sec ; 2.341 cm/yr or 23.41mm/yr
#r^2=0.009756

a.1 <- crms_ele[grep("355",crms_ele$Station.ID),]
lm.ele <- lm(a.1$Ver.Pin.ht.cm~as.POSIXct(a.1$datetime))
#3.819e-08 cm/sec 1.204 cm/yr      r2=0.6258
a <- crms.ht[grep("355",crms.ht$Station.ID),]
a$datetime <- as.POSIXct(a$datetime)
a.obs <- filter(waterht.obs, site=="CO")
a.obs <- a.obs[,c("END", "INLAND_WAT_DEPTH_FT")]
a.obs$In_Wat_dep_cm <- a.obs$INLAND_WAT_DEPTH_FT*30.48
a.obs$watlvl <- a[match(a.obs$END,a$datetime),"Adjusted.Water.ele.datum..cm."]
a.obs$rel.marsh.ele.cm <- a.obs$watlvl-a.obs$In_Wat_dep_cm
a.obs$marsh.ele.inc.cm <- lm.ele$coefficients[2]*as.integer(a.obs$END)+lm.ele$coefficients[1]
a.obs <- arrange(a.obs, END)

a3 <- t.test(a.obs$marsh.ele.inc,a.obs$rel.marsh.ele.cm,paired = T)
lm.ele$coefficients[1]-a3$conf.int[1]

a$marsh.ele.cm <- (as.integer(a$datetime)*lm.ele$coefficients[2])+abs(lm.ele$coefficients[1])-a3$conf.int[1]
a$adj.water.ele.to.marsh.cm <- a$Adjusted.Water.ele.datum..cm.-a$marsh.ele.cm
#
tb.crms.ht <- a[,c("Station.ID","datetime","marsh.ele.cm","adj.water.ele.to.marsh.cm")]

#######TB-PLOT WL####
#plot
plot(a$Adjusted.Water.ele.datum..cm.~a$datetime, type="l",cex=.2,col="grey30",main="Terrebonne Bay hourly water level,\n Average Water Level, CRMS Marsh Elevation, and Calculated Marsh Elevation \n CRMS Station #0355 from 2006-2016")
points(a$marsh.ele.cm~a$datetime, cex=.2, col="red")
points((a$Adjusted.Water.ele.datum..cm.-(a$Adjusted.Water.Elevation.to.Marsh..ft.*30.48))~a$datetime, col="green",cex=.2)
abline(lm(a$Adjusted.Water.ele.datum..cm.~a$datetime), cex=.2, col="blue")
#
#abline(a=-47.62106,b=3.818964e-08, col="red")
#####################Port Sulphur############################ 
a <- crms.ht[grep("4529", crms.ht$Station.ID),] #ps
b <- crms.ht[grep("4529", crms.ht$Station.ID),]
b <- subset(b,datetime >= as.POSIXct("2012-01-01") & 
            b$datetime<= as.POSIXct('2015-01-01 00:00'))


nrow(a)/length(seq(as.POSIXct("2007-07-25 14:00"),as.POSIXct("2016-10-17 12:00"), by = "hours"))
#coverage=85.5%

a$index <- index(a$datetime)
lm <- lm(a$Adjusted.Water.ele.datum..cm.~as.integer(a$datetime)) #long term
#avg wat lvl change = 3.188e-08  cm/sec; 1.01 cm/yr | 10.1mm/yr
#r^2=0.01833
lm.b <- lm(b$Adjusted.Water.ele.datum..cm.~as.integer(b$datetime))
#avg wat lvl change = 2.843e-09 ft/sec; 2.733 cm/yr | 27.33mm/yr
#r^2=0.0165


a.1 <- crms_ele[grep("4529",crms_ele$Station.ID),]
lm.ele <- lm(a.1$Ver.Pin.ht.cm~as.POSIXct(a.1$datetime))
# 1.369e-08 cm/sec  0.43 cm/yr        r2=0.131
a <- crms.ht[grep("4529",crms.ht$Station.ID),]
a$datetime <- as.POSIXct(a$datetime)
a.obs <- filter(waterht.obs, site=="PS")
a.obs <- a.obs[,c("END", "INLAND_WAT_DEPTH_FT")]
a.obs$In_Wat_dep_cm <- a.obs$INLAND_WAT_DEPTH_FT*30.48
a.obs$watlvl <- a[match(a.obs$END,a$datetime),"Adjusted.Water.ele.datum..cm."]
a.obs$rel.marsh.ele.cm <- a.obs$watlvl-a.obs$In_Wat_dep_cm
a.obs$marsh.ele.inc.cm <- lm.ele$coefficients[2]*as.integer(a.obs$END)+lm.ele$coefficients[1]
a.obs <- arrange(a.obs, END)

a3 <- t.test(a.obs$marsh.ele.inc,a.obs$rel.marsh.ele.cm,paired = T)
lm.ele$coefficients[1]-a3$estimate

a$marsh.ele.cm <- (as.integer(a$datetime)*lm.ele$coefficients[2])+abs(lm.ele$coefficients[1])-a3$estimate
a$adj.water.ele.to.marsh.cm <- a$Adjusted.Water.ele.datum..cm.-a$marsh.ele.cm
#
ps.crms.ht <- a[,c("Station.ID","datetime","marsh.ele.cm","adj.water.ele.to.marsh.cm")]

#####PS-Plot WL#####
#plot
plot(a$Adjusted.Water.ele.datum..cm.~a$datetime, type="l",cex=.2,col="grey30",main="Port Sulphur hourly water level,\n Average Water Level, CRMS Marsh Elevation, and Calculated Marsh Elevation \n CRMS Station #4529 from 2006-2016")
points(a$marsh.ele.cm~a$datetime, cex=.2, col="red")
points((a$Adjusted.Water.ele.datum..cm.-(a$Adjusted.Water.Elevation.to.Marsh..ft.*30.48))~a$datetime, col="green",cex=.2)
abline(lm(a$Adjusted.Water.ele.datum..cm.~a$datetime), cex=.2, col="blue")
#
#abline(a=-2.176438 ,b=1.368757e-08, col="red")
########
ggplot(crms_ele, aes(datetime, ))

################Bind Linear regression and elevation###################
a <- bind_rows(tb.crms.ht,gi.crms.ht,ps.crms.ht)
a <- arrange(a,Station.ID,datetime)
a$match <- paste0(a$Station.ID,"-",a$datetime)
crms.ht$match <- paste0(crms.ht$Station.ID,"-",crms.ht$datetime)
a <- a[match(a$match,crms.ht$match),]
crms.ht <- cbind(crms.ht,a[,3:4])
crms.ht[,c("match")] <- NULL

rm("a","a.obs","a2","a3","a4","tb.crms.ht","gi.crms.ht","ps.crms.ht","lm","waterht.obs","lm.ele","a.1")


ggplot(crms.ht, aes(datetime,Adjusted.Water.ele.datum..cm.,group=Station.ID))+
  geom_line(color="grey30",alpha=.9)+geom_point(aes(datetime,marsh.ele.cm),color="orange",size=.1)+
  geom_smooth(aes(group=Station.ID),method="lm", se=F, color="blue")+
  facet_wrap(~Station.ID,nrow = 3)+theme_light()+theme(legend.position="none")+
  scale_y_continuous(limits =  c(-50,100))+
  labs(y=" Water Level Adjusted to Datum (cm)",x="",
       title="Water Level throughout Louisiana Marshes\nAverage Water Level and Marsh Elevation \nJuly 2006 - October 2016")+
  theme(plot.title = element_text(size=18,face="bold"),
        panel.border = element_rect(color="black",size = 2),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=14,color = "black",angle = 45,hjust = 1),
        legend.title = element_text(size=16, face="bold.italic"),
        legend.text = element_text(size=12),
        legend.position = c(.14,.85),
        legend.background = element_rect(size=1,color="black"),
        strip.text.x = element_text(size=14,color="black", face = "bold.italic"))
#
ggsave("./Figures/WatELE-MarshELE_CRMS_all.pdf",plot=last_plot(),scale=1,
       width=13,height=7)


################Fourier and Linear Regression##################
cmgi <- data.frame(datetime = crms_gi$datetime,
                   indextime = index(crms_gi$datetime),
                   watlvl = crms_gi$Adjusted.Water.Level..ft.)
cmtb <- data.frame(datetime = crms_tb$datetime,
                   indextime = index(crms_tb$datetime),
                   watlvl = crms_tb$Adjusted.Water.Level..ft.)
cmps <- data.frame(datetime = crms_ps$datetime,
                   indextime = index(crms_ps$datetime),
                   watlvl = crms_ps$Adjusted.Water.Level..ft.)
cmgi <- cmgi[complete.cases(cmgi$watlvl),]
t <- index(cmgi$datetime)
ssp <- spectrum(cmgi$watlvl)
per=1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(cmgi$watlvl~sin(2*pi/per*t)+cos(2*pi/per*t)+
              sin(4*pi/per*t)+cos(4*pi/per*t))
lm <- lm(cmgi$watlvl~cmgi$datetime)
#plot(cmgi$watlvl~cmgi$datetime, type="l")
#lines(fitted(reslm)~cmgi$datetime, col="red", lty=2,cex=3)
#abline(lm(cmgi$watlvl~cmgi$datetime), col="blue")
#
ggplot(cmgi, aes(datetime,watlvl))+geom_point(size=.5,color="grey50")+
  geom_line(aes(datetime,fitted(reslm)),color="red",size=2)+
  geom_abline(intercept = lm$coefficients[1],slope = lm$coefficients[2], size=1)+
  theme_light()+labs(title="Grand Isle (CRMS-0165) hourly water level \n with 2 fitted Fourier Series",
                     x="",y="Water Level (ft)")+
  annotate("text",label="Linear Regression\n r^2=0.146\n\n Fourier Series r^2=0.057",
           x=as.POSIXct("2013-10-10 12:00"),y=15,size=5)+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 16, face = "bold"))+
  theme(plot.title=element_text(size=17, face="bold"))
ggsave(file=paste0("GI_Regression_",Sys.Date(),"_01.pdf"), 
       plot = last_plot(), scale=1, dpi = 300,width = 14,height=11)

#
cmps <- cmps[complete.cases(cmps$watlvl),]
t <- index(cmps$datetime)
ssp <- spectrum(cmps$watlvl)
per=1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(cmps$watlvl~sin(2*pi/per*t)+cos(2*pi/per*t)+
              sin(4*pi/per*t)+cos(4*pi/per*t))
lm <- lm(cmps$watlvl~cmps$datetime)
#plot(cmps$watlvl~t, type="l", col="gray")
#lines(fitted(reslm)~t, col="red", lty=2, lwd=4)
#abline(lm(cmps$watlvl~cmps$datetime), col="blue)

ggplot(cmps, aes(datetime,watlvl))+geom_point(size=.5,color="grey50")+
  geom_line(aes(datetime,fitted(reslm)),color="red",size=2)+
  geom_abline(intercept = lm$coefficients[1],slope = lm$coefficients[2], size=1)+
  theme_light()+labs(title="Port Sulphur (CRMS-4529) hourly water level \n with Linear Regression and 2 fitted Fourier Series",
                     x="",y="Water Level (ft)")+
  annotate("text",label="Linear Regression\n r^2=0.0499\n\n Fourier Series r^2=0.113",
           x=as.POSIXct("2013-10-10 12:00"),y=5,size=5)+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 16, face = "bold"))+
  theme(plot.title=element_text(size=17, face="bold"))
ggsave(file=paste0("PS_CRMS_Regression_",Sys.Date(),"_01.pdf"), 
       plot = last_plot(), scale=1, dpi = 300,width = 14,height=11)
##
cmtb <- cmtb[complete.cases(cmtb$watlvl),]
t <- index(cmtb$datetime)
ssp <- spectrum(cmtb$watlvl)
per=1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(cmtb$watlvl~sin(2*pi/per*t)+cos(2*pi/per*t)+
              sin(4*pi/per*t)+cos(4*pi/per*t)+
              sin(6*pi/per*t)+cos(6*pi/per*t)+
              sin(8*pi/per*t)+cos(8*pi/per*t))
lm <- lm(cmtb$watlvl~cmtb$datetime)
#plot(cmtb$watlvl~cmtb$datetime, type="l")
#lines(fitted(reslm)~cmtb$datetime, col="red", lty=2)
#abline(lm(cmtb$watlvl~cmtb$datetime), col="blue)

ggplot(cmtb, aes(datetime,watlvl))+geom_point(size=.5,color="grey50")+
  geom_line(aes(datetime,fitted(reslm)),color="red",size=2)+
  geom_abline(intercept = lm$coefficients[1],slope = lm$coefficients[2], size=1)+
  theme_light()+labs(title="Terrebonne Bay (CRMS-0355) Hourly Water level \n with Linear Regression and 4 fitted Fourier Series",
                     x="",y="Water Level (ft)")+
  annotate("text",label="Linear Regression\n r^2=0.0186\n\n Fourier Series r^2=0.3985",
           x=as.POSIXct("2009-10-10 12:00"),y=7.5,size=5)+
  theme(axis.text.x = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 16, face = "bold"))+
  theme(plot.title=element_text(size=17, face="bold"))
ggsave(file=paste0("TB_CRMS_Regression_",Sys.Date(),"_01.pdf"), 
       plot = last_plot(), scale=1, dpi = 300,width = 14,height=11)

###############MII-GI######################
e <- as.POSIXct(c("2006-07-02 00:00:00", "2016-10-16 00:00:00"), format = "%Y-%m-%d %H:%M")
f <- seq.POSIXt(e[1], e[2], by = "24 hours")
df <- data.frame(date = f)
df2 <- crms.ht[grep("164",crms.ht$Station.ID),]
#
df$match <- match(df[,"date"], df2[,"datetime"], incomparables = NULL)
for(x in 1:23) {
  df <- cbind(df, df[,"match"]-x)
} 
for(x in 2:25) {
  df <- cbind(df, df2[df[,x],"adj.water.ele.to.marsh.cm"])
}

df <- df[,-c(2:25)]
colnames(df) <- c("date", paste("hour", "-",1:24, sep = ""))

a <- apply(sign(df[,2:25]),1, function(x) table(factor(x, levels=c(-1,0,1))))
a <- t(a)
df <- cbind(df, a)
colnames(df)[26:28] <- c("neg", "zero", "pos")
df$zero <- apply(df, 1, function(x) sum(length(x[is.na(x)])))
df$max <- apply(df[,2:25], 1, function(x) max(x,na.rm = T))
df$zero <- df$zero/24

df$MII <- with(df, ifelse(zero >= .5, z <- NA,
                          ifelse(pos <5 & max <=10, z <- 0,
                                 ifelse(pos <=12 & max <=20, z <- 1,
                                        ifelse(pos >=13 & max <=30, z <- 2,
                                               ifelse(pos >=15 & max >30 & max <45, z <- 3,
                                                      ifelse(pos >=15 & max >=45, z <- 4,
                                                             z <- 1)))))))

#hist(df$MII)
#ggplot(df, aes(date, MII))+
#  geom_point()+
#  geom_smooth(method = "auto")
mii_gi <- data.frame(date=df$date,
                     mii= df$MII,
                     neg=df$neg,
                     pos=df$pos,
                     max=df$max)


########MII-TB####
e <- as.POSIXct(c("2006-07-02 00:00:00", "2016-10-16 00:00:00"), format = "%Y-%m-%d %H:%M")
f <- seq.POSIXt(e[1], e[2], by = "24 hours")
df <- data.frame(date = f)
df2 <- crms.ht[grep("355",crms.ht$Station.ID),]
#
df$match <- match(df[,"date"], df2[,"datetime"], incomparables = NULL)
for(x in 1:23) {
  df <- cbind(df, df[,"match"]-x)
} 
for(x in 2:25) {
  df <- cbind(df, df2[df[,x],"adj.water.ele.to.marsh.cm"])
}

df <- df[,-c(2:25)]
colnames(df) <- c("date", paste("hour", "-",1:24, sep = ""))

a <- apply(sign(df[,2:25]),1, function(x) table(factor(x, levels=c(-1,0,1))))
a <- t(a)
df <- cbind(df, a)
colnames(df)[26:28] <- c("neg", "zero", "pos")
df$zero <- apply(df, 1, function(x) sum(length(x[is.na(x)])))
df$max <- apply(df[,2:25], 1, function(x) max(x,na.rm = T))
df$zero <- df$zero/24

df$MII <- with(df, ifelse(zero >= .5, z <- NA,
                          ifelse(pos <5 & max <=10, z <- 0,
                                 ifelse(pos <=12 & max <=20, z <- 1,
                                        ifelse(pos >=13 & max <=30, z <- 2,
                                               ifelse(pos >=15 & max >30 & max <45, z <- 3,
                                                      ifelse(pos >=15 & max >=45, z <- 4,
                                                             z <- 1)))))))

#hist(df$MII)
#ggplot(df, aes(date, MII))+
#  geom_point()+
#  geom_smooth(method = "auto")
mii_tb <- data.frame(date=df$date,
                     mii= df$MII,
                     neg=df$neg,
                     pos=df$pos,
                     max=df$max)

###############MII-PS#########
e <- as.POSIXct(c("2007-07-27 00:00:00", "2016-10-17 00:00:00"), format = "%Y-%m-%d %H:%M")
f <- seq.POSIXt(e[1], e[2], by = "24 hours")
df <- data.frame(date = f)
df2 <- crms.ht[grep("4529",crms.ht$Station.ID),]
df$match <- match(df[,"date"], df2[,"datetime"], incomparables = NULL)
for(x in 1:23) {
  df <- cbind(df, df[,"match"]-x)
} 
for(x in 2:25) {
  df <- cbind(df, df2[df[,x],"adj.water.ele.to.marsh.cm"])
}

df <- df[,-c(2:25)]
colnames(df) <- c("date", paste("hour", "-",1:24, sep = ""))

a <- apply(sign(df[,2:25]),1, function(x) table(factor(x, levels=c(-1,0,1))))
a <- t(a)
df <- cbind(df, a)
colnames(df)[26:28] <- c("neg", "zero", "pos")
df$zero <- apply(df, 1, function(x) sum(length(x[is.na(x)])))
df$max <- apply(df[,2:25], 1, function(x) max(x,na.rm = T))
df$zero <- df$zero/24

df$MII <- with(df, ifelse(zero >= .5, z <- NA,
                          ifelse(pos <5 & max <=10, z <- 0,
                                 ifelse(pos <=12 & max <=20, z <- 1,
                                        ifelse(pos >=13 & max <=30, z <- 2,
                                               ifelse(pos >=15 & max >30 & max <45, z <- 3,
                                                      ifelse(pos >=15 & max >=45, z <- 4,
                                                             z <- 1)))))))
#hist(df$MII)
#ggplot(df, aes(date, MII))+
#  geom_point()+
#  geom_smooth(method = "auto")
mii_ps <- data.frame(date=df$date,
                     mii= df$MII,
                     neg=df$neg,
                     pos=df$pos,max=df$max)

##############MII-Combine & plot############
mii_gi$source <- "CRMS0164"
mii_ps$source <- "CRMS4529"
mii_tb$source <- "CRMS0355"
a <- bind_rows(mii_gi, mii_ps, mii_tb)
a <- arrange(a,source,date)
a <- na.omit(a)
hist(a$mii)
describe(a$mii)
a$source <- factor(a$source, levels=c("CRMS4529","CRMS0164","CRMS0355"),
                   labels=c("CRMS4529-Port Sulphur","CRMS0164-Grand Isle",
                            "CRMS0355-Terrebonne Bay"),ordered = T)
crms_mii <- a
a <-setNames(c("PS","GI","TB"), c("CRMS4529-Port Sulphur","CRMS0164-Grand Isle",
                                  "CRMS0355-Terrebonne Bay"))
crms_mii$region <- a[crms_mii$station]

rm("a","mii_gi","mii_ps","mii_tb","e","f","df","df2")
#####plot MII#######

ggplot(crms_mii, aes(mii, group=station))+
  geom_histogram(aes(group=station, fill=station),color="grey20", binwidth = 1)+
  facet_wrap(~station, nrow=1)+theme_light()+scale_y_continuous(limits=c(0,1500), breaks= seq(0,1500,250),expand = c(0,0))+
  labs(y="Count of 24-Hour Intervals",x="Marsh Inundation Index (MII)",
       title="Marsh Inundation Index\nJuly 2006 - October 2016")+
  theme(plot.title = element_text(size=18,face="bold"),
        panel.border = element_rect(color="black",size = 2),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.title.x = element_text(size=16),
        #legend.title = element_text(size=16, face="bold.italic"),
        #legend.text = element_text(size=12),
        legend.position = "none",
        #legend.background = element_rect(size=1,color="black"),
        strip.text.x = element_text(size=14,color="black", face = "bold"))
#
ggsave("./Figures/MII_region_20170309.pdf",plot=last_plot(),scale=1,
       width=10,height=8)
  

##############################Facts###############
crms.ht$datetime <- as.POSIXlt(crms.ht$datetime)
crms.ht$day <- crms.ht$datetime$mday
crms.ht$month <- crms.ht$datetime$mon+1
crms.ht$year <- crms.ht$datetime$year+1900
crms.ht$month <- factor(crms.ht$month, levels = c(seq(1,12,1)),
                        labels=c(month.name))
crms.ht$datetime <- as.POSIXct(crms.ht$datetime)


a <- ddply(crms.ht,.(Station.ID,year),summarize,
           n=length(adj.water.ele.to.marsh.cm),
           mean=mean(adj.water.ele.to.marsh.cm),
           pos=100*(sum(adj.water.ele.to.marsh.cm>0)/n),
           neg = 100*(sum(adj.water.ele.to.marsh.cm<0)/n))
write.csv(a,"Crms_annual_facts.csv")

####Dataset for pub####
write.csv(cmb.dep, "./Datasets/Core_microbiome_DEPTH_20170111.csv")
#####################
crms_mii$month <-  factor(strftime(as.POSIXlt(crms_mii$date),format="%m"))
crms_mii$year <- factor(strftime(as.POSIXlt(crms_mii$date),format="%Y"))
a <- melt(crms_mii, id=c("date","year","region"),measure.vars = "mii")
a <- ddply(a,.(year,region,variable),summarize,
           N=length(value),
           sd=sd(value),
           se=sd/sqrt(N),
           mean=mean(value))
#a$moyr <- paste0(a$month,"-",01,"-",a$year)
#a$date <- as.POSIXct(a$moyr, format="%m-%d-%Y")
a$region <- factor(a$region, levels = c("PS","GI","TB"),ordered = T)


ggplot(a, aes(year,mean))+
  geom_line(aes(group=1),size=1)+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.3,size=.7,color="gray30")+
  geom_point(size=3,shape=15,color="blue")+
  facet_wrap(~region)+ scale_y_continuous(limits=c(0,3),breaks = seq(0,3,1))+
  theme_light()+
  labs(#title="Mean Monthly Precipitation in Southern Louisiana\n1983-2015 from NADP Site-LA30",
    y="MII",x=NULL)+
  theme(plot.title = element_text(size=18,face="bold"),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,color = "black",angle = 45,hjust = 1),
        legend.title = element_text(size=16,face="bold.italic"),
        strip.text.x = element_text(size=14,color="black", face = "bold"),
        panel.border = element_rect(color="black",size = 2),
        #legend.background = element_rect(size=.5,color="black"),
        #legend.text = element_text(size = 14))+
        legend.position = "none")
ggsave("./Figures/MII_monthly_20170212.pdf",plot=last_plot(),device = pdf,width = 10,height=7.5)
 



