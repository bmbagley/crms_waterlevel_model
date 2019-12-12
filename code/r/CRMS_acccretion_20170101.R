library(plyr)
library(dplyr)
library(reshape2)
library(zoo)
library(ggplot2)
#
##################Elevation Data##################
crms_ele <- read.csv("C:/Users/Brandon/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_Surface_Elevation_All.csv", sep = ",", strip.white = T,
                     check.names = T, header = T, stringsAsFactors = F)
crms_ele <- Filter(function(x)!all(is.na(x)), crms_ele)
sites <- c("CRMS0355-E01","CRMS0164-E01","CRMS4529-E01")
crms_ele <- filter(crms_ele,   Station.ID %in%sites) 
rm("sites")
crms_ele$datetime <- as.POSIXlt(paste0(crms_ele$Sample.Date..mm.dd.yyyy.," ",
                                       crms_ele$Sample.Time..hh.mm.), 
                                format = "%m/%d/%Y %H:%M")

crms_ele$monthyear <- paste0(crms_ele$datetime$year+1900,"-",
                             crms_ele$datetime$mon+1)
crms_ele$year <- crms_ele$datetime$year+1900
crms_ele$Ver.Pin.ht.cm <- crms_ele$Verified.Pin.Height..mm./10

#####################
a <- crms_ele[,c("Sample.Date..mm.dd.yyyy.","Station.ID","Verified.Pin.Height..mm.")]
a <- ddply(a,.(Sample.Date..mm.dd.yyyy.,Station.ID),summarize,
           N=length(Verified.Pin.Height..mm.),
           sd=sd(Verified.Pin.Height..mm.),
           se=sd/sqrt(N),
           mean=mean(Verified.Pin.Height..mm.),
           max=max(Verified.Pin.Height..mm.),
           min=min(Verified.Pin.Height..mm.))
a <- na.omit(a)
a$Station.ID <- factor(a$Station.ID, levels = c("CRMS4529-E01","CRMS0164-E01", "CRMS0355-E01"),
                       labels = c("CRMS4529-Port Sulphur",
                                  "CRMS0164-Grand Isle","CRMS0355-Terrebonne Bay"))
colnames(a)[1:2] <- c("Date","Station")
a$Date <- as.POSIXct(a$Date, format="%m/%d/%Y")
a$mean <- a$mean/10

ggplot(a, aes(Date,mean,color=Station))+
  geom_line(aes(group=Station),size=1)+
  scale_x_datetime(date_labels = "%b %Y",date_breaks= "6 months")+
  geom_point(aes(shape=Station,group=Station),size=3)+
  theme_light()+
  labs(title="Time Series of Marsh Surface Elevation\n2006-2016 from CRMS Stations",
       y="Marsh Elevation Above Datum (cm)",x="")+
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
ggsave("./Figures/MonthlyELEVATION_CRMS_all.pdf",plot=last_plot(),scale=1,
       width=13,height=7)
##########??????######################

b <- data.frame(watlvl = (crms.ht[grep("355",crms.ht$Station.ID),"Adjusted.Water.ele.datum..cm."]),
                watlvl.date = (crms.ht[grep("355",crms.ht$Station.ID),"datetime"]),
                marsh.ele = lm$coefficients[2]*as.numeric(b$watlvl.date)-lm$coefficients[1],
                marsh.ele.resid = (crms.ht[grep("355",crms.ht$Station.ID),
    "Adjusted.Water.ele.datum..cm."])-(lm$coefficients[2]*as.numeric(b$watlvl.date)-lm$coefficients[1]))
                


a.obs <- filter(waterht.obs, site=="GI")
a.obs <- a.obs[,c("END", "INLAND_WAT_DEPTH_FT")]
a.obs$index <- match(a.obs$END,a$datetime)
a2 <- lm$residuals[a.obs$index]
a2 <- data.frame(resid = a2, obs = a.obs$INLAND_WAT_DEPTH_FT)
a3 <- t.test(a2$resid,a2$obs,paired = T)
a4 <- t.test(a2$obs,a[match(a.obs$END,a$datetime),"Adjusted.Water.Elevation.to.Marsh..ft."],paired = T)

t <- a$index
ssp <- spectrum(a$Adjusted.Water.Level..ft.)
per=1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(a$Adjusted.Water.Level..ft.~sin(2*pi/per*t)+cos(2*pi/per*t)+
              sin(4*pi/per*t)+cos(4*pi/per*t))

a$marsh.ele.ft <- (as.integer(a$datetime)*lm$coefficients[2])+lm$coefficients[1]+a3$estimate
a$adj.water.ele.to.marsh.ft <- a$Adjusted.Water.Level..ft.-a$marsh.ele.ft

gi.crms.ht <- a[,c("Station.ID","datetime","marsh.ele.ft","adj.water.ele.to.marsh.ft")]

plot(crms.ht[grep("355",crms.ht$Station.ID),"Adjusted.Water.ele.datum..cm."]~crms.ht[grep("355",crms.ht$Station.ID),"datetime"],type="l")
abline(lm(a$pinht.cm~a$datetime),col="blue")
abline(lm(crms.ht[grep("355",crms.ht$Station.ID),"Adjusted.Water.ele.datum..cm."]~crms.ht[grep("355",crms.ht$Station.ID),"datetime"]),col="red")
##############Linear Regression of Elevation Data#####################

###All sampling period
a <- crms_ele[grep("335",crms_ele$Station.ID),]
a$datetime <- as.POSIXct(a$datetime)
lm.tb <- lm(a$Ver.Pin.ht.cm~as.integer(a$datetime))
summary(lm)
#3.270e-07 mm/sec
#10.31227 mm/yr elevation change - TB, r2=0.6699
a <- crms_ele[grep("164",crms_ele$Station.ID),]
a$datetime <- as.POSIXct(a$datetime)
summary(lm(a$Verified.Pin.Height..mm.~a$datetime))
#8.066909 mm/yr elevation chnage - GI, r2=0.5822
a <- crms_ele[grep("4529",crms_ele$Station.ID),]
a$datetime <- as.POSIXct(a$datetime)
summary(lm(a$Verified.Pin.Height..mm.~a$datetime))
#4.317278 mm.yr elevation change - PS, r2=0.131

####2012-2014
a <- crms_ele[,c("Sample.Date..mm.dd.yyyy.","Station.ID","Verified.Pin.Height..mm.")]
a$Sample.Date..mm.dd.yyyy. <- as.POSIXct(a$Sample.Date..mm.dd.yyyy., format = "%m/%d/%Y")
a <- subset(a,Sample.Date..mm.dd.yyyy. >= as.POSIXct("2012-01-01") & 
              a$Sample.Date..mm.dd.yyyy.<= as.POSIXct('2015-01-01'))
a <- na.omit(a)
#
b <- a[grep("0164", a$Station.ID),]
summary(lm(b$Verified.Pin.Height..mm.~b$Sample.Date..mm.dd.yyyy.)) #2.561e-07 mm/sec 
#0.807637 cm/yr elevation change 2012-2014 - Grand Isle, r^2=0.1432
b <- a[grep("0355", a$Station.ID),]
summary(lm(b$Verified.Pin.Height..mm.~b$Sample.Date..mm.dd.yyyy.)) #1.409e-07
#0.4443422 cm/yr elevation change 2012-2014 - Terrebonne Bay, r^2 = 0.07597
b <- a[grep("4529", a$Station.ID),]
summary(lm(b$Verified.Pin.Height..mm.~b$Sample.Date..mm.dd.yyyy.)) #8.097e-08
#0.255347 cm/yr elevation change 2012-2014 - Port Sulphur, r^2 = 0.009003


#####Accretion########################
crms_acc <- read.csv("C:/Users/Brandon/Google Drive/UTK/GOMRI_Data/Env Data/CRMS/CRMS_Full_Accretion.csv", sep = ",", strip.white = T,
                     check.names = T, header = T, stringsAsFactors = F)
crms_acc <- Filter(function(x)!all(is.na(x)), crms_acc)
crms_acc <- filter(crms_acc, grepl("CRMS0355|CRMS0164|CRMS4529",Station.ID)) 

crms_acc$datetime <- as.POSIXct(paste0(crms_acc$Sample.Date..mm.dd.yyyy.," ",
                                       crms_acc$Sample.Time..hh.mm.), 
                                format = "%m/%d/%Y %H:%M")
crms_acc$Sample.Date..mm.dd.yyyy. <- as.POSIXct(crms_acc$Sample.Date..mm.dd.yyyy.,
                                                format = "%m/%d/%Y")
##
a <- crms_acc[,c("Sample.Date..mm.dd.yyyy.","Station.ID","Accretion.Measurement.1..mm.",
                 "Accretion.Measurement.2..mm.","Accretion.Measurement.3..mm.",
                 "Accretion.Measurement.4..mm.")]
a <- melt(a,id.vars =c("Sample.Date..mm.dd.yyyy.","Station.ID"))
a$Station.ID <- substr(a$Station.ID,1,8)
a <- na.omit(a)
a$value <- a$value/10
a <- ddply(a,.(Sample.Date..mm.dd.yyyy.,Station.ID),summarize,
           N=length(value),
           sd=sd(value),
           se=sd/sqrt(N),
           mean=mean(value))
a$Station.ID <- factor(a$Station.ID, levels = c("CRMS4529","CRMS0164", "CRMS0355"),
                       labels = c("CRMS4529-Port Sulphur",
                                  "CRMS0164-Grand Isle","CRMS0355-Terrebonne Bay"))
colnames(a)[1:2] <- c("Date","Station")
a <- na.omit(a)
#
ggplot(a, aes(Date,mean,color=Station))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=2e6,size=.5,color="grey40")+
  geom_line(aes(group=Station),size=1)+
  scale_x_datetime(date_labels = "%b %Y",date_breaks= "6 months")+
  geom_point(aes(shape=Station,group=Station),size=3)+
  theme_light()+
  labs(title="Time Series of Marsh Sediment Accretion\n2006-2016 from CRMS Stations",
       y="Vertical Accretion above Marker Horizon (cm)",x="")+
  theme(plot.title = element_text(size=18,face="bold"),
        panel.border = element_rect(color="black",size = 2),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=14,color = "black",angle = 45,hjust = 1),
        legend.title = element_text(size=16, face="bold.italic"),
        legend.text = element_text(size=12),
        legend.position = c(.14,.85),
        legend.background = element_rect(size=1,color="black"),
        strip.text.x = element_text(size=14,color="black", face = "bold.italic"))+
  geom_smooth(aes(group=Station),method = "lm",se=F)
#
ggsave("./Figures/MonthlyAccretion_CRMS_all.pdf",plot=last_plot(),scale=1,
       width=13,height=7)
##
ggplot(a, aes(Date,mean,color=Station))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=2e6,size=.5,color="grey40")+
  geom_line(aes(group=Station),size=1)+
  geom_smooth(aes(group=Station),method="lm", color="black")+
  facet_wrap(~Station,ncol=1,scales = "free_x")+
  scale_x_datetime(date_labels = "%b %Y",date_breaks= "6 months",expand = c(0,0))+
  geom_point(aes(shape=Station,group=Station),size=3)+
  theme_light()+
  labs(title="Time Series of Marsh Sediment Accretion\n2006-2016 from CRMS Stations",
       y="Vertical Accretion above Marker Horizon (cm)",x="")+
  theme(plot.title = element_text(size=18,face="bold"),
        panel.border = element_rect(color="black",size = 2),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=14,color = "black",angle = 45,hjust = 1),
        legend.position= "none",
        #legend.title = element_text(size=16, face="bold.italic"),
        #legend.text = element_text(size=12),
        #legend.position = c(.14,.85),
        #legend.background = element_rect(size=1,color="black"),
        strip.text.x = element_text(size=14,color="black", face = "bold.italic"))
#
ggsave("./Figures/MonthlyAccretion_CRMS_facet.pdf",plot=last_plot(),scale=1,
       width=13,height=12)

#######Linear Reg of Accretion#####
a <- crms_acc[,c("Sample.Date..mm.dd.yyyy.","Station.ID","Accretion.Measurement.1..mm.",
                 "Accretion.Measurement.2..mm.","Accretion.Measurement.3..mm.",
                 "Accretion.Measurement.4..mm.")]
a <- melt(a,id.vars =c("Sample.Date..mm.dd.yyyy.","Station.ID"))
a$Station.ID <- substr(a$Station.ID,1,8)
a <- na.omit(a)
a$value <- a$value/10
#data from plot
b <- a[grep("355",a$Station.ID),]
summary(lm(b$value~b$Sample.Date..mm.dd.yyyy.))
plot(b$value~b$Sample.Date..mm.dd.yyyy., type="p")
abline(lm(b$value~b$Sample.Date..mm.dd.yyyy.),col="blue")
#3.437e-08 mm/sec *(60*60*24*365)/10
#0.1083892 cm/yr accretion - TB, r2=0.195
b <- a[grep("164",a$Station.ID),]
lm <- lm(b$value~b$Sample.Date..mm.dd.yyyy.)
summary(lm)
plot(b$value~b$Sample.Date..mm.dd.yyyy., type="p")
abline(lm(b$value~b$Sample.Date..mm.dd.yyyy.),col="blue")
#1.160e-08 mm/sec
#0.0366 cm/yr accretion - GI, r2=0.123
b <- a[grep("4529",a$Station.ID),]
lm <- lm(b$value~b$Sample.Date..mm.dd.yyyy.)
summary(lm)
plot(b$value~b$Sample.Date..mm.dd.yyyy., type="p")
abline(lm(b$value~b$Sample.Date..mm.dd.yyyy.),col="blue")
#1.172e-08 mm/sec
#0.0369 cm/yr accretion - PS, r2=0.02944