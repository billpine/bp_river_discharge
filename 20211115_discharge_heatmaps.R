
#SETUP
rm(list=ls());
library("waterData")
library("hydroTSM")
library("zoo")


#station to analyze
station = '02358000'   

################################################################################ 
#
#                     READ AND PREPARE DATA
#
################################################################################ 
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1920-10-01") 
#ok looks like temp code is wrong for this station, maybe rainfall is available?

#stat codes herehttps://help.waterdata.usgs.gov/stat_code

#get some date components
dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#make dataset from epochs, 
disE  = dis[dis$dates>='1920-10-01' & dis$dates<='2021-11-30',]  

#get monthly sum, mean, sd, and var
#discharge
disE.mo  = aggregate(val~month+year,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))
disE.mo  = do.call('data.frame',disE.mo)
names(disE.mo)[3:6] = c('avg','sd','var','sumflow') 
disE.mo$yrmo = disE.mo$year+(disE.mo$month-0.5)/12       

#value just by month
disE.month  = aggregate(val~month,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))
disE.month  = do.call('data.frame',disE.month)
names(disE.month)[2:5] = c('avg','sd','var','sumflow')

#calculate how far each month average is from the overall average - absolute difference
for(i in 1:nrow(disE.mo)){
        m = disE.mo$month[i]
        disE.mo$diff[i] = disE.mo$avg[i] - disE.month$avg[disE.month$month == m]
        disE.mo$diff_percent[i] = ((disE.mo$avg[i] - disE.month$avg[disE.month$month == m])/disE.month$avg[disE.month$month == m])*100
}

#create a matrix with months and years as the rows/cols for absolute values
library(reshape2)
dis_mat = dcast(disE.mo, year ~ month, value.var = "diff")
dis_mat2 = as.matrix(dis_mat[,2:ncol(dis_mat)], dimnames = list(rownames = dis_mat$year, colnames = colnames(dis_mat)))
rownames(dis_mat2) <- dis_mat$year
#create matrix for percentage
dis_per = dcast(disE.mo, year ~ month, value.var = "diff_percent")
dis_per2 = as.matrix(dis_per[,2:ncol(dis_per)], dimnames = list(rownames = dis_mat$year, colnames = colnames(dis_mat)))
rownames(dis_per2) <- dis_per$year

#plot as a heat map
require(plot.matrix)
library(RColorBrewer)
min(dis_mat2, na.rm = T) #-25608.03
max(dis_mat2, na.rm = T) #133243.6
par(oma=c(0.5,0.5,0.5,3))
mypallette <- brewer.pal(4, "RdBu")
plot(dis_mat2, xlab = "Month", ylab = "Year",
     breaks = c(-30000, -10000, 0, 10000, 200000),
     fmt.key="%.0f",
     main = "Discharge",
     col = mypallette)

#plot percentages
col2 <- brewer.pal(9, "Blues") #for the +%s
col3 <- brewer.pal(11, "RdBu") #red is - blue is +
colAll <- c(col3[1], col3[2], col3[3], col3[6], col2[4:7], col3[10],col2[8:9], col3[11], "#042333ff") 
par(oma=c(0.5,0.5,0.5,3))
plot(dis_per2, xlab = "Month", ylab = "Year",
     breaks = c(-100, -50, -25, -10, 10, 25, 50, 100, 150, 200, 250, 300, 350, 400, 450,500),
     main = "Deviations in Discharge from Period of Record",
     axis.row = list(las = 2),
     col = colAll, na.col = "black")

#subset for just the year 2005-2020
include_years <- as.character(as.numeric(2005:2021))
dis_per20052020 <- dis_per2[include_years, ]
plot(dis_per20052020, xlab = "Month", ylab = "Year",
     breaks = c(-100, -50, -25, -10, 10, 25, 50, 100, 150, 200, 250, 300, 350, 400, 450),
     main = "Deviations in Discharge from Period of Record",
     axis.row = list(las = 2),
     col = colAll, na.col = "black")


#subset for just the year 2005-2012
include_years <- as.character(as.numeric(2005:2012))
dis_per2005 <- dis_per2[include_years, ]
plot(dis_per2005, xlab = "Month", ylab = "Year",
     breaks = c(-100, -50, -25, -10, 10, 25, 50, 100, 150, 200, 250, 300, 350, 400, 450),
     main = "Deviations in Discharge from Period of Record",
     axis.row = list(las = 2),
     col = colAll, na.col = "black")

#subset for just the years 2013-2020
include_years <- as.character(as.numeric(2013:2021))
dis_per2013 <- dis_per2[include_years, ]
plot(dis_per2013, xlab = "Month", ylab = "Year",
     breaks = c(-100, -50, -25, -10, 10, 25, 50, 100, 150, 200, 250, 300, 350, 400),
     main = "Deviations in Discharge from Period of Record",
     axis.row = list(las = 2),
     col = colAll, na.col = "black")
