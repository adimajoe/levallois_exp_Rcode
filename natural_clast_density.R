setwd("/Users/aditimajoe/Desktop/IISER/levallois_experiment")
getwd()
#load libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(devtools)

#import dataset as csv file

raw_clasts_data <- read.csv("/Users/aditimajoe/Desktop/IISER/levallois_experiment/natural_clasts_metrics_combined.csv")

summary(raw_clasts_data$raw_material)

data_quartzite <- raw_clasts_data %>% filter(raw_material=="Q")
levels(droplevels(data_quartzite$raw_material))

str(data_quartzite$raw_material)

#add volume column

data_quartzite$clast_volume <- (data_quartzite$length*data_quartzite$breadth)*data_quartzite$thickness

#add density column

data_quartzite$clast_density <- data_quartzite$weight/data_quartzite$clast_volume

#density~site

data_quartzite %>%
  group_by(site) %>%
  summarise_at(vars(clast_density), list(name = mean))

#IQR
r <- data_quartzite %>% 
  group_by(site) %>%
  summarise(IQR(clast_density))
r <- data.frame(r)

ggplot(data_quartzite, aes(x=site, y=clast_density))+
  geom_boxplot(fill="slateblue", alpha=0.2)+
  xlab("Site") + ylab("Clast Density")

aov.model.site <- aov(data_quartzite$clast_density~data_quartzite$site)
summary(aov.model.site)

kruskal.test(data_quartzite$clast_density~data_quartzite$site)

TukeyHSD(aov.model.site)

#density~colour

data_quartzite %>%
  group_by(colour) %>%
  summarise_at(vars(clast_density), list(name = mean))

#IQR
r <- data_quartzite %>% 
  group_by(site) %>%
  summarise(IQR(clast_density))
r <- data.frame(r)

ggplot(data_quartzite, aes(x=colour, y=clast_density))+
  geom_boxplot(fill="lightgreen", alpha=0.2)+
  xlab("Site") + ylab("Clast Density")+
  theme_bw()

aov.model <- aov(data_quartzite$clast_density~data_quartzite$colour)
summary(aov.model)

kruskal.test(data_quartzite$clast_density~data_quartzite$colour)

TukeyHSD(aov.model)

#remove density outliers from dataset (everything above 0.05)

data_quartzite_no_outliers <- data_quartzite[-which(data_quartzite$clast_density > 0.005),]
data_quartzite_no_outliers <- data_quartzite_no_outliers[-which(data_quartzite_no_outliers$length < 5.0),]


data_quartzite_no_outliers %>%
  group_by(site) %>%
  summarise_at(vars(clast_density), list(name = mean))


ggplot(data_quartzite_no_outliers, aes(x=site, y=clast_density))+
  geom_boxplot(fill="slateblue", alpha=0.2)+
  xlab("Site") + ylab("Clast Density")

kruskal.test(data_quartzite_no_outliers$clast_density~data_quartzite_no_outliers$site)

aov.model.site <- aov(data_quartzite_no_outliers$clast_density~data_quartzite_no_outliers$site)
summary(aov.model.site)

TukeyHSD(aov.model.site)

ggplot(data_quartzite_no_outliers, aes(x=colour, y=clast_density))+
  geom_boxplot(fill="slateblue", alpha=0.2)+
  xlab("Colour") + ylab("Clast Density")

#Kruskaal Wallis for colour relationship

kruskal.test(data_quartzite_no_outliers$clast_density~data_quartzite_no_outliers$colour)

TukeyHSD(aov.model.colour)

#texture summary

str(data_quartzite_no_outliers$texture)
table(data_quartzite_no_outliers$texture)

kruskal.test(data_quartzite_no_outliers$clast_density~data_quartzite_no_outliers$texture)

#remove density outliers according to IQR method
#1 make different datasets for every site
#2 find IQR, Q3 and Q1 for every site
#Remove outliers using "Outliers = Observations > Q3 + 1.5*IQR  or < Q1 – 1.5*IQR"
#merge the datasets together

data_gnbk<-data_quartzite_no_outliers %>% filter(site=="GNBK")
data_gsbd<-data_quartzite_no_outliers %>% filter(site=="GSBD")
data_kkr<-data_quartzite_no_outliers %>% filter(site=="KKR")
data_mrch<-data_quartzite_no_outliers %>% filter(site=="MRCH")
data_sjnp<-data_quartzite_no_outliers %>% filter(site=="SJNP")
data_sjnpd<-data_quartzite_no_outliers %>% filter(site=="SJNP-D")

#GNBK

quantile(data_gnbk$clast_density, prob=c(.25,.5,.75), type=1)

0.001504630+ 1.5*(0.001504630-0.001304366) = 0.001805026
0.001304366- 1.5*(0.001504630-0.001304366) = 0.00100397

data_gnbk_changed<- data_gnbk[-which(data_gnbk$clast_density < 0.00100397),]
data_gnbk_changed<- data_gnbk_changed[-which(data_gnbk_changed$clast_density > 0.001805026),]

#GSBD

quantile(data_gsbd$clast_density, prob=c(.25,.5,.75), type=1)

r <- data_gsbd %>% 
  summarise(IQR(clast_density))
r <- data.frame(r)

0.002281947+1.5*(0.0004633508) = 0.002976973

0.001816441-1.5*(0.0004633508) = 0.001121415


data_gsbd_changed<- data_gsbd[-which(data_gsbd$clast_density < 0.001121415),]
data_gsbd_changed<- data_gsbd_changed[-which(data_gsbd_changed$clast_density > 0.002976973),]

#KKR

quantile(data_kkr$clast_density, prob=c(.25,.5,.75), type=1)

0.002493351+1.5*(0.002493351-0.001923218) = 0.003348551
0.001923218-1.5*(0.002493351-0.001923218) = 0.001068018

data_kkr_changed<- data_kkr[-which(data_gsbd$clast_density < 0.001068018),]
data_kkr_changed<- data_kkr_changed[-which(data_kkr_changed$clast_density > 0.003348551),]

#MRCH

quantile(data_mrch$clast_density, prob=c(.25,.5,.75), type=1)

r <- data_mrch %>% 
  summarise(IQR(clast_density))
r <- data.frame(r)

0.002204472+1.5*(0.002204472-0.001778589) = 0.002843296
0.001778589-1.5*(0.002204472-0.001778589) = 0.001139764

data_mrch_changed<- data_mrch[-which(data_mrch$clast_density > 0.003348551),]

#SJNP
quantile(data_sjnp$clast_density, prob=c(.25,.5,.75), type=1)

0.002055138+1.5*(0.002055138-0.001620520) = 0.002707065
0.001620520-1.5*(0.002055138-0.001620520) = 0.000968593


data_sjnp_changed<- data_sjnp[-which(data_sjnp$clast_density < 0.000968593),]
data_sjnp_changed<- data_sjnp_changed[-which(data_sjnp_changed$clast_density > 0.002707065),]

#SJNPD
quantile(data_sjnpd$clast_density, prob=c(.25,.5,.75), type=1)

0.002170434+1.5*(0.002170434-0.001753407) = 0.002795975
0.001753407-1.5*(0.002170434-0.001753407) = 0.001127866

#all the observations already fall in this category
data_sjnpd_changed<- data_sjnpd

#merge all the datasets

data_merged <-rbind(data_gnbk_changed,data_gsbd_changed,data_kkr_changed,data_mrch_changed,
                    data_sjnp_changed, data_sjnpd_changed)

#density~site

data_merged %>%
  group_by(site) %>%
  summarise_at(vars(clast_density), list(name = mean))

ggplot(data_merged, aes(x=site, y=clast_density))+
  geom_boxplot(fill="slateblue", alpha=0.2)+
  xlab("Site") + ylab("Clast Density")

aov.model.site <- aov(data_merged$clast_density~data_merged$site)
summary(aov.model.site)

kruskal.test(data_merged$clast_density~data_merged$site)

TukeyHSD(aov.model.site)


