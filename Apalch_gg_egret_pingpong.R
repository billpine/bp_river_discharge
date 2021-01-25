#R exercise with USFWS-AZFRO to 
#(1) Read in data using waterData package
#(2) Make graphs using ggplot
#(3) Work with data to subset using dplyr
#(4) Make simple summary tables

#https://owi.usgs.gov/R/training-curriculum/intro-curriculum/AnalyzeII/


#this is one way to install packages, however, I like to use the
#package tab on the bottom right window in RStudio.

install.packages("lubridate","tidyverse","waterData","dplyr","Hmisc")

#after you install a package once on your computer the package is
#there, so you don't have to keep installing it, but you do have
#to turn it on as I do below.

library("lubridate")
library("tidyverse")
library("waterData")
library("dplyr")
library("Hmisc")

#station to analyze

#here is something neat, you can just change which station
#you want to plot by changing the station number for the USGS
#gage below where it says station = '09402300'
#remember the data are coming from the server so it make take a few
#seconds to download depending on the internet speed and number of years
#of data

#Run code line by line by placing cursor at end of line and clicking run


#so we are going to work with the data from the LCR near gage near 
#Boulders camp.  I think at the place called "gage pool"

#Apalachicola
station = '02358000'   

#if I wanted to work with Cameron gage I would just change the number
#I do this I keep a few gages just saved in my code here and if I wanted to work
#with Cameron instead I would put a # on before the word station on line
#34 and then remote the # before the word station on line 41
#LCR Cameron
#station = '09402000'

#Colorado near Desert View
#station = '09402500'   

#get site name to use in plot titles and such
stinfo  = siteInfo(station)
stname = stinfo$staname

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1922-07-01")

str(dis)
summary(dis)

#code are standards from USGS 00060=discharge, 00065=gage height, 00010= Temp
#stat USGS standards 00001 max, 00003 mean daily
#see examples https://owi.usgs.gov/R/dataRetrieval.html#7

#what are the names of our columns?
names(dis)

#use dplyr to rename
#the pattern is new_name= old_name

dis<-rename(dis, discharge = val)

names(dis)

#now let's removed any erroneous or estimated values or missing

#what values are there
unique(dis$qualcode)

#let's remove NA or only keep "A"

#remove NA 
#dis <- filter(dis, qualcode != "NA")
#!= is "not equal to" so filter what is not equal to NA

#only keep A
#dis <- filter(dis, qualcode == "A")
#== is "equal to" something

#just check
unique(dis$qualcode)

####################
#ok use ggplot to make a plot
####################

names(dis)

#make a dot plot of discharge by year

# ggplot(dis, aes(dates, discharge)) +
#   geom_point() +
#   ggtitle("Little Colorado River Discharge (CFS) Near Boulders") +
#   xlab("Year") + ylab("Discharge (Cubic Feet per Second)")

#OR USE THE NAME FROM THE GAGE
#this way we don't have to keep typing it when we change data (change gage)

ggplot(dis, aes(dates, discharge)) +
  geom_point() +
  ggtitle(stname) +
  xlab("Year") + ylab("Discharge (Cubic Feet per Second)")

#quickly we see there are a few values over 2000 CFS, obvious floods. Most data are
#very low discharge

#really simple histogram
ggplot(dis, aes(x=discharge)) +
  geom_histogram() +
  ggtitle(stname) +
  xlab("Discharge (Cubic Feet per Second)")

#just adjust the bins and the x limit
ggplot(dis, aes(x=discharge)) +
  geom_histogram(binwidth=1000) +
  ggtitle(stname) +
  xlim(0,200000)+
  xlab("Discharge (Cubic Feet per Second)")

#now let's create a new variable called "year" and extract year from date
#if we use str(dis) we see date is recognized as a date, so just use lubridate

dis$year<-year(dis$dates)

#using label=TRUE gives us the month as a word
dis$month <-month(dis$dates,label=TRUE)

##need to convert from numeric to character month
head(dis)

#ok make a dot plot by year

ggplot(dis, aes(month, discharge)) +
  geom_point() +
  ggtitle(stname) +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)") +
  facet_wrap(~year)+

  theme_set(theme_gray(base_size = 18))

#add bootstrap mean and 95% CI to plot

ggplot(dis, aes(month, discharge)) +
  geom_point() +
  ggtitle(stname) +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)") +
  facet_wrap(~year) +
  stat_summary(fun.data = "mean_cl_boot",colour = "red", size = 0.5)

#what if we only want to plot 2019?

dis_2019<-filter(dis,year=2019)

ggplot(dis_2019, aes(month, discharge)) +
  geom_point() +
  ggtitle(stname) +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)") +
  stat_summary(fun.data = "mean_cl_boot",colour = "red", size = 1.5)

#############






############

# Creating box plots

#all data by month
ggplot(dis, aes(month, discharge)) +
  geom_boxplot()+
  ggtitle(stname) +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)")+
  ylim(0,200000)

#Mel way
ggplot(dis_2019, aes(month, discharge))+ 
  geom_point(aes(colour=factor(year))) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", size= 3, shape=20, color="red", fill="red") +
  ggtitle(stname) +
  xlab("Month") +
  ylab("Discharge (Cubic Feet per Second)") +
  labs(colour= "Year") +
  theme_classic()

#Stephen way

dis_post<-filter(dis,year>2012)


#greater than 2012
mean_annual_dis_post <- aggregate(discharge ~ month + year, dis_post, mean) 
#
ggplot(dis, aes(month, discharge)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(data = mean_annual_dis_post, aes(x=month, y=discharge, colour=factor(year))) + # remove 'size' argument from aes function
  guides(color = guide_legend(override.aes = list(size = 10))) +  # change legend icon size
  ggtitle("Apalachicola River @ Chattahoochee 2013-2020") +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)") +
  scale_y_continuous(limits=c(0, 100000),
                     breaks = c(0,5000,10000,50000,75000,100000),
                     expand = c(0, 0)) +
  theme_set(theme_gray(base_size = 25)) +
  labs(col="Year") +                                                         # rename legend title
  theme(legend.title = element_text(size=25),          # legend title size
        legend.text = element_text(size=20),          # legend text size 
        legend.key = element_rect(fill='white'))     # remove gray background in legend

#


#years 2005-2012

dis_pre<-filter(dis,year<2013 & year>2004)

mean_annual_dis_pre <- aggregate(discharge ~ month + year, dis_pre, mean) 

#
ggplot(dis, aes(month, discharge)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(data = mean_annual_dis_pre, aes(x=month, y=discharge, colour=factor(year))) + # remove 'size' argument from aes function
  guides(color = guide_legend(override.aes = list(size = 10))) +  # change legend icon size
  ggtitle("Apalachicola River @ Chattahoochee 2005-2012") +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)") +
  scale_y_continuous(limits=c(0, 100000),
                     breaks = c(0,5000,10000,50000,75000,100000),
                     expand = c(0, 0)) +
  theme_set(theme_gray(base_size = 25)) +
  labs(col="Year") +                                                         # rename legend title
  theme(legend.title = element_text(size=25),          # legend title size
        legend.text = element_text(size=20),          # legend text size 
        legend.key = element_rect(fill='white'))     # remove gray background in legend

#



#all data by month and year
ggplot(dis, aes(month, discharge)) +
  geom_boxplot()+
  ggtitle(stname) +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)")+
  facet_wrap(~year)

#if you want to put on log scale 
#+scale_y_log10()+


#limit y axis to range by month and year
ggplot(dis, aes(month, discharge)) +
  geom_boxplot()+
  ggtitle(stname) +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)") +
  ylim(0,1000)+
  facet_wrap(~year)
#+scale_y_log10()+





#what about summary stats?

############################
##make this into nice table?
##fix digits?
############################

#by year
summarise_discharge_yr<-dis%>%
  group_by(year)%>%
  summarise(mean=mean(discharge,na.rm=TRUE),
            std_dev=sd(discharge, na.rm=TRUE))

print.data.frame(round(summarise_discharge_yr, digits = 2))

write.table(summarise_discharge_yr, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)

#by year & month
summarise_discharge_yr_month<-dis%>%
  group_by(year,month)%>%
  summarise(mean=mean(discharge,na.rm=TRUE),
            std_dev=sd(discharge, na.rm=TRUE))

#bootstrap way for mean and CI
report_table<-summarise_discharge_ci<-dis%>%
  group_by(year,month)%>%
  summarise(ci = list(mean_cl_boot(discharge) %>% 
                        rename(mean=y, lwr=ymin, upr=ymax))) %>% 
  unnest(cols=c(ci))

#now make another plot with the bootstrap mean and CI
ggplot(report_table, aes(month, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  ggtitle(stname) +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)") +
  facet_wrap(~year)



#lowest discharge ?
min(dis$discharge)
#3900

#when did that happen?
dis[dis$discharge==3900, ]

