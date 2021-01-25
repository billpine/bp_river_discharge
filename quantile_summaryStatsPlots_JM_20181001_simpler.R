### Transect data analysis

library("lubridate")

#read in data
transect<- read.csv("transect_data.csv", header= T)

#create year, month, day columns
transect$date<-mdy(transect$date)
transect$year<- year(transect$date)
transect$month<-month(transect$date)
transect$day<-day(transect$date)

#create season column
transect$Season<-ifelse(transect$month ==1 | transect$month == 10 | transect$month == 11 | transect$month == 12, "Winter", "Summer")

#Not all of the observations have time, so time would not parse all of the values
transect$date<- ymd(with(transect, paste(year,month,day, sep="-")))

#remove rows with NA for count_live
transect2 <- transect[!is.na(transect$count_live),]

#remove rows with NA for tran_length
transect2 <-transect[!is.na(transect$tran_length),]


########################
## Summary Statistics ##
########################

#aggregate data for each transect 
dta=aggregate(count_live~day+month+year+Season+treatment+locality+site+bar+station,data=transect2,sum)
#any transects with all 0s - yes, 6 of 256! (2%)

#aggregate data for transect length
dta2=aggregate(tran_length~day+month+year+Season+treatment+locality+site+bar+station,data=transect2,max)

#merge data frames
dta3=merge(dta,dta2,by=c("day","month","year","Season","treatment","locality","site","bar","station"))

#calculate density
dta3$area = dta3$tran_length*.154
dta3$density = dta3$count_live/dta3$area

#summary statistics function
sumstats = function(x){ 
    NoTran0=length(na.omit(x[x==0]))
    NoTranLive=length(na.omit(x[x>0]))
    NobsTotal=length(na.omit(x))
    y=na.omit(x[x>0])
    Mean=mean(y) 
    Median=median(y)
    Sd=sd(y) 
    Var=var(y)
    CV=sd(y)/mean(y)
    Se=sd(y)/sqrt(length(y))
    L95se=mean(y)-1.96*(sd(y)/sqrt(length(y)))
    U95se=mean(y)+1.96*(sd(y)/sqrt(length(y)))
    bstrap <- c()
    for (i in 1:1000){
      bstrap <- c(bstrap, mean(sample(y,(length(y)),replace=T), na.rm = T))}
    Bstrapmean = mean(bstrap)
    L95bstrap = quantile(bstrap,.025)
    U95bstrap = quantile(bstrap,.975)
    return(list(NobsTotal=NobsTotal, Mean = Mean, Median=Median, Sd = Sd, Var = Var, CV = CV, Se = Se, L95se = L95se, U95se = U95se, Bstrapmean = Bstrapmean, L95bstrap = L95bstrap, U95bstrap = U95bstrap))
}


stat_overall = sumstats(dta3$density)

stat_year = list()
for(i in as.character(min(dta3$year):max(dta3$year)))
{
  if(length(dta3$density[dta3$year == i]) > 0)
    stat_year[[i]] = sumstats(dta3$density[dta3$year == as.numeric(i)])
}

stat_season = list()
for(i in c("Winter", "Summer")){
  stat_season[[i]] = sumstats(dta3$density[dta3$Season == i])
}

stat_location = list()
for(i in levels(dta3$locality)){
  stat_location[[i]] = sumstats(dta3$density[dta3$locality == i])
}




###################
## Summary Plots ##
###################

#overall boxplot of density
boxplot(dta3$density, ylab = "Density", main = "Oyster Densities Overall")

#boxplot of density by locality
boxplot(dta3$density ~ dta3$locality, ylab = "Density", xlab = "Localities", main = "Oyster Densities by Localities")

#boxplot of density by season
boxplot(dta3$density ~ dta3$Season, ylab = "Density", xlab = "Season", main = "Oyster Densities by Season")

#boxplot of density by treatment
boxplot(dta3$density ~ dta3$treatment, ylab = "Density", xlab = "Treatment", main = "Oyster Densities by Treatment")

#boxplot of density by site
boxplot(dta3$density ~ dta3$site, ylab = "Density", xlab = "Site", main = "Oyster Densities by Site")
        
#boxplot of density by station
boxplot(dta3$density ~ dta3$station, ylab = "Density", xlab = "Station", main = "Oyster Densities by Station")

#boxplot of density by month
boxplot(dta3$density ~ dta3$month, ylab = "Density", xlab = "Month", main = "Oyster Densities by Month")

#boxplot of density by year
boxplot(dta3$density ~ dta3$year, ylab = "Density", xlab = "Year", main = "Oyster Densities by Year")

#histogram of density by locality
hist(dta3$density[dta3$locality == "CK"], xlab = "Density", main = "Oyster Density Histogram at Locality CK")
hist(dta3$density[dta3$locality == "CR"], xlab = "Density", main = "Oyster Density Histogram at Locality CR")
hist(dta3$density[dta3$locality == "HB"], xlab = "Density", main = "Oyster Density Histogram at Locality HB")
hist(dta3$density[dta3$locality == "LC"], xlab = "Density", main = "Oyster Density Histogram at Locality LC")
hist(dta3$density[dta3$locality == "LT"], xlab = "Density", main = "Oyster Density Histogram at Locality LT")

###########################
## Lone Cabbage Analysis ##
###########################

#Locality == LC == Lone Cabbage
LC <- dta3[dta3$locality == "LC",]
#101 rows of data -> 101 transects
#data from 2010 - 2017

#number of oysters
boxplot(LC$count_live, ylab = "Oyster Live Counts", main = "Oyster Live Counts Overall")
#range 0 - 2488

#density
boxplot(LC$density, ylab = "Density", main = "Oyster Densities Overall")
#range 0 - 737.71

#by season
boxplot(LC$density ~ LC$Season, ylab = "Density", xlab = "Season", main = "Oyster Densities by Season")
#slighty higher in winter
t.test(LC$Season == "Summer", LC$Season == "Winter")
#not significant

#by treatment
boxplot(LC$density ~ LC$treatment, ylab = "Density", xlab = "Treatment", main = "Oyster Densities by Treatment")
#higher for rocks than control
t.test(LC$treatment == "control", LC$treatment == "rocks")
#significant

#by date
LC$date<- ymd(with(LC, paste(year,month,day, sep="-")))
plot(LC$date[LC$treatment == "control"], LC$density[LC$treatment == "control"], col = "blue", pch = 2, xlab = "Date", ylab = "Density", main = "Oyster Densities over Time")
points(LC$date[LC$treatment == "rocks"], LC$density[LC$treatment == "rocks"], col = "red", pch = 8)
legend("topright", pch = c(2,8), col = c("blue", "red"), legend = c("Control", "Rocks"))








