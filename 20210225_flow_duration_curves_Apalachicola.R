

################################################################################
#
# this code reads daily water guage data from a USGS site, computes the weekly
# composite mean discharge, the weekly climatology over the entire time frame,
# and anomolies based on the the time span of climatoloty.  Requires the
# waterData package.  Visit the following pages for site numbers and metadata
#
# sites:       http://maps.waterdata.usgs.gov/mapper/
# stat codes:  http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table
# code codes:  http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes     (choose physical)
#
# Wilcox Suwannee  gauge 02323500
#Chattahoochee gauge 02358000
#
################################################################################
#SETUP
rm(list=ls())
library("waterData")
library("hydroTSM")
library("zoo")

#station to analyze
station = '02358000'   

#Chattahoochee gauge 02358000
#02322500 is Suwannee Wilcox

################################################################################ 
#
#                     READ AND PREPARE DATA
#
################################################################################ 
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1930-10-01") 
#ok looks like temp code is wrong for this station, maybe rainfall is available?

#stat codes herehttps://help.waterdata.usgs.gov/stat_code

#get some date components
dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#make dataset from epochs, 
disE  = dis[dis$dates>='1930-10-01' & dis$dates<='2020-12-31',]  





################################################################################ 
#
#                     PLOT DATA
#
################################################################################ 

#--------------------------- duration curves -----------------------------------
#simply converting each vector to 'ts' allows to cbind without recycling values in shorter vector
par(mfcol=c(1,2))

#subset for years of interest
dis.fdc = as.data.frame(cbind(ts(disE$val[disE$year<2021]),ts(disE$val[disE$year>2007&disE$year<2013]),
                              ts(disE$val[disE$year > 2012])))


#create myself (not using fdc() function)
all <- sort(dis.fdc$`ts(disE$val[disE$year < 2021])`, decreasing = T)
part <- sort(dis.fdc$`ts(disE$val[disE$year > 2007 & disE$year < 2013])`,decreasing = T)
part2 <- sort(dis.fdc$`ts(disE$val[disE$year > 2012])`, decreasing = T)

df_all <- data.frame(x = 100/length(all) * 1:length(all), y = all)
df_part_2008_2012 <- data.frame(x = 100/length(part) * 1:length(part), y = part)
df_part_2013_2020 <- data.frame(x = 100/length(part2) * 1:length(part2), y = part2)

#make plots
plot(x = df_all$x, y = log(df_all$y), type = 'l', lwd = 2, ylab = "Log(Flow (cf/s))", 
     xlab = "% Time flow equalled or exceeded", xaxt = 'n',main='Discharge duration')
lines(x = df_part_2008_2012$x, y = log(df_part_2008_2012$y), type = 'l', col = 'red', lwd = 2)
abline(h = log(5000), lwd = 2, col = 'blue', lty = 2)
legend("topright", legend = c("1930-2020", "2008-2012"), lty = 1, col = c("black", "red"))
axis(1, labels = c("0%", "20%", "40%", "60%", "80%", "100%"), at = c(0, 20, 40, 60,80, 100))

plot(x = df_all$x, y = log(df_all$y), type = 'l', lwd = 2, ylab = "Log(Flow (cf/s))", 
     xlab = "% Time flow equalled or exceeded", xaxt = 'n',main='Discharge duration')
lines(x = df_part_2013_2020$x, y = log(df_part_2013_2020$y), type = 'l', col = 'red', lwd = 2)
legend("topright", legend = c("1930-2020", "2013-2020"), lty = 1, col = c("black", "red"))
abline(h = log(5000), lwd = 2, col = 'blue', lty = 2)
axis(1, labels = c("0%", "20%", "40%", "60%", "80%", "100%"), at = c(0, 20, 40, 60, 80, 100))


#how often is the 2005-2012 line below the period of record line
#x column is different for each data set
#lets round x column to nearest .00 
df_all2 <- df_all
df_all2$x <- round(df_all2$x, digits = 2)
df_all3 <- aggregate(df_all2, by = list(df_all2$x), FUN = 'mean')

df_part_2008_2012_2 <- df_part_2008_2012
df_part_2008_2012_2$x <- round(df_part_2008_2012_2$x, digits = 2)
df_part_2008_2012_3 <- aggregate(df_part_2008_2012_2, by = list(df_part_2008_2012_2$x), FUN = 'mean')

df_merge  <- merge(df_part_2008_2012_3, df_all3, by.x = "x", by.y = "x")

length(which(log(df_merge$y.x) < log(df_merge$y.y)))
length(df_merge$y.x)

#0.955 (1744/1927) #this depends on which gauge and years used

#how often is the 2013-2020 line below the period of record line
df_part_2013_2020_2 <- df_part_2013_2020
df_part_2013_2020_2$x <- round(df_part_2013_2020_2$x, digits = 2)
df_part_2013_2020_3 <- aggregate(df_part_2013_2020_2, by = list(df_part_2013_2020_2$x), FUN = 'mean')

df_merge2  <- merge(df_part_2013_2020_3, df_all3, by.x = "x", by.y = "x")

length(which(log(df_merge2$y.x) < log(df_merge2$y.y)))
length(df_merge2$y.x)

#1422/2922 = 48.67%
